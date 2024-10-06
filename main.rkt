#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
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
  [define-xs:type    define-schema-type]
  [define-xs:import  import-schema]
  [make-xs:schema    xs:schema]
  [make-xs:import    xs:import]
  [make-xs:type      xs:type]
  [make-xs:all       xs:all]
  [make-xs:sequence  xs:sequence]
  [make-xs:choice    xs:choice])
 unbounded
 xs
 tns
 xs:min-inclusive
 xs:min-exclusive
 xs:max-inclusive
 xs:max-exclusive
 xs:pattern
 xs:length
 xs:min-length
 xs:max-length
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


(define-syntax (make-xs:type stx)
  (syntax-parse stx
    #:datum-literals (enum range)

    ;; simple type
    [(_ base e_i ...)
     #'(xs:simple-type base (list e_i ...))]

    ;; integer range
    [(_ (range lo:integer hi:integer))
     #'(xs:simple-type
        xs:integer
        (list (xs:min-inclusive lo)
              (xs:max-inclusive hi)))]

    ;; decimal range
    [(_ (range lo:number hi:number))
     #'(xs:simple-type
        xs:decimal
        (list (xs:min-inclusive lo)
              (xs:max-inclusive hi)))]

    ;; enum
    [(_ (enum s_i:str ...))
     #'(xs:simple-type
        xs:string
        (list (xs:enumeration s_i) ...))]

    ;; complex type
    [(_ ([a_i:id t_i:id r_i:boolean] ...) e)
     #'(xs:complex-type
        (apply hash (append (list 'a_i (xs:attribute (tns t_i) r_i)) ...))
        e)]))

(define-syntax (make-xs:all stx)
  (syntax-parse stx
    [(_ (x_i:id e_i) ...)
     #'(xs:all
        (apply (append (list 'x_i e_i) ...)))]))
         
(define-syntax (make-xs:sequence stx)
  (syntax-parse stx
    [(_ (x_i:id e_i) ...)
     #'(xs:sequence
        (apply (append (list 'x_i e_i) ...)))]))
         
(define-syntax (make-xs:choice stx)
  (syntax-parse stx
    [(_ lo hi (x_i:id e_i) ...)
     #'(xs:choice
        lo
        hi
        (apply (append (list 'x_i e_i) ...)))]))
         

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








