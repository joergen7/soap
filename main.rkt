#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax
           let
           #%datum)
  (only-in syntax/parse
           syntax-parse
           boolean
           id
           str
           integer
           number))
 (only-in typed/xml
          xexpr->string
          display-xml
          document
          prolog
          xexpr->xml
          element?
          empty-tag-shorthand)
 (only-in racket/set
          set)
 "xml-aux.rkt"
 "xml-schema.rkt"
 "xml-wsdl.rkt")

(provide
 #%top-interaction
 #%app
 #%top
 #%datum
 #%module-begin
 in-namespace
 (rename-out
  [display-xs:schema display-schema]
  [define-xs:schema  define-schema]
  [define-xs:type    define-type]
  [define-xs:import  import]
  [make-xs:all       all]
  [make-xs:sequence  sequence]
  [make-xs:choice    choice]
  [xs:min-inclusive  min-inclusive]
  [xs:min-exclusive  min-exclusive]
  [xs:max-inclusive  max-inclusive]
  [xs:max-exclusive  max-exclusive]
  [xs:pattern        pattern]       ; also in syntax/parse
  [xs:length         length]        ; name-clash with length from racket/base
  [xs:min-length     min-length]
  [xs:max-length     max-length]
  [xs:element        element])

 range
 enum
 unbounded
 xs
 tns
 )


;; reader module

(module reader syntax/module-reader
  soap)

;; parameters

(define a-namespace : (Parameterof String)
  (make-parameter "urn:default"))

(define a-import-table : (Parameterof (HashTable Symbol (U xs:import xs:schema)))
  (let ([t0 : (HashTable Symbol (U xs:import xs:schema)) (hash)])
    (make-parameter t0)))

(define a-type-table : (Parameterof (HashTable Symbol xs:schema-member))
  (let ([t0 : (HashTable Symbol xs:schema-member) (hash)])
    (make-parameter t0)))


;; language extensions

(define-syntax (store stx)
  (syntax-parse stx
    [(_ param:id key:id value)
     #'(param (hash-set (param) 'key value))]))

(define-syntax (store-type stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-type-table key value)]))

(define-syntax (store-import stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-import-table key value)]))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ s:str)
     #'(a-namespace s)]))

(define-syntax (define-xs:schema stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(define x : xs:schema
         (make-xs:schema e_i ...))]))

(define-syntax (make-xs:schema stx)
  (syntax-parse stx
    [(_ e_i ...)
     #'(parameterize ([a-import-table (hash)]
                      [a-type-table   (hash)])
         e_i ...
         (xs:schema
          (a-namespace)
          (a-import-table)
          (a-type-table)))]))

(define-syntax (define-xs:import stx)
  (syntax-parse stx
    
    ;; local import
    [(_ x:id a:id)
     #'(store-import x a)]

    ;; foreign import
    [(_ x:id e ...)
     #'(store-import x (make-xs:import e ...))]))


(define-syntax (make-xs:import stx)
  (syntax-parse stx
    
    ;; foreign import without declarations
    [(_ s:str)
     #'(make-xs:import s ())]
    
    ;; Foreign import with declarations
    [(_ s:str (a_i:id ...))
     #'(xs:import s (set 'a_i ...))]))
        

(define-syntax (define-xs:type stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(store-type
        x
        (make-xs:type e_i ...))]))

(: range (-> Real Real xs:simple-type))
(define (range lo hi)
  (xs:simple-type
   (if (and (exact-integer? lo)
            (exact-integer? hi))
       xs:integer
       xs:decimal)
   (list (xs:min-inclusive lo)
         (xs:max-inclusive hi))))

(: enum (-> String * xs:simple-type))
(define (enum . s-list)
  (xs:simple-type
   xs:string
   (map xs:enumeration s-list)))

(: make-xs:simple-type (-> (U (-> qname) xs:simple-type) xs:simple-type-member * xs:simple-type))
(define (make-xs:simple-type head . tail)
  (cond
    [(xs:simple-type? head)
     head]
    [else
     (xs:simple-type head tail)]))


(define-syntax (make-attribute-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol xs:attribute) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-element-table r ...) 'x e)]))

(define-syntax (make-element-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol xs:element) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-element-table r ...) 'x e)]))

(define-syntax (make-xs:type stx)
  (syntax-parse stx

    ;; complex type
    [(_ ((a_i:id t_i r_i:boolean) ...) e)
     #'(xs:complex-type
        (make-attribute-table (a_i (xs:attribute t_i r_i)) ...)
        e)]

    ;; simple type
    [(_ head e_i ...)
     #'(make-xs:simple-type head e_i ...)]))


                               

(define-syntax (make-xs:all stx)
  (syntax-parse stx
    [(_ r ...)
     #'(xs:all
        (make-element-table r ...))]))
         
(define-syntax (make-xs:sequence stx)
  (syntax-parse stx
    [(_ r ...)
     #'(xs:sequence
        (make-element-table r ...))]))
         
(define-syntax (make-xs:choice stx)
  (syntax-parse stx
    [(_ lo hi r ...)
     #'(xs:choice
        lo
        hi
        (make-element-table r ...))]))

(define-syntax (display-xexpr stx)
  (syntax-parse stx
    [(_ e)
     #'(parameterize ([empty-tag-shorthand 'always])
         (display-xml
          (document (prolog '() #f '())
                    (assert (xexpr->xml e) element?)
                    '())))]))

(define-syntax (display-xs:schema stx)
  (syntax-parse stx
    ([_ e]
     #'(display-xexpr (xs->xexpr e)))))

;; TODO
;; - proper error message on hash-ref flunk
;; - no redefining schemas
;; - no name redefining prefixes on import








