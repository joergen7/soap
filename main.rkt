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
 unbounded
 xs
 tns
 (rename-out
  [display-xs:schema        display-schema]
  [display-wsdl:definitions display-service]
  [define-xs:schema         define-schema]
  [define-xs:type           define-type]
  [define-xs:import         import]
  [make-xs:all              all]
  [make-xs:sequence         sequence]
  [make-xs:choice           choice]
  [xs:min-inclusive         min-inclusive]
  [xs:min-exclusive         min-exclusive]
  [xs:max-inclusive         max-inclusive]
  [xs:max-exclusive         max-exclusive]
  [xs:pattern               pattern]
  [xs:length                length]
  [xs:min-length            min-length]
  [xs:max-length            max-length]
  [xs:range                 range]
  [xs:enum                  enum]
  [define-wsdl:definitions  define-service]
  [define-wsdl:port-type    define-interface]
  [define-wsdl:message      define-message]
  [make-wsdl:operation      operation]
  [make-qname               :]
  [wsdl:part                part]
  ))


;; reader module

(module reader syntax/module-reader
  soap)

;; parameters

(define a-namespace : (Parameterof String)
  (make-parameter "urn:default"))

(define a-import-table : (Parameterof (HashTable Symbol (U xs:import xs:schema)))
  (let ([t0 : (HashTable Symbol (U xs:import xs:schema)) (hash)])
    (make-parameter t0)))

(define a-schema-table : (Parameterof (HashTable Symbol xs:schema-member))
  (let ([t0 : (HashTable Symbol xs:schema-member) (hash)])
    (make-parameter t0)))

(define a-wsdl-table : (Parameterof (HashTable Symbol wsdl:definitions-member))
  (let ([t0 : (HashTable Symbol wsdl:definitions-member) (hash)])
    (make-parameter t0)))


;; language extensions

(define-syntax (resolve stx)
  (syntax-parse stx
    [(_ x:id) #'(tns x)]
    [(_ x)    #'x]))

(define-syntax (store stx)
  (syntax-parse stx
    [(_ param:id key:id value)
     #'(param (hash-set (param) 'key value))]))

(define-syntax (store-schema stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-schema-table key value)]))

(define-syntax (store-import stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-import-table key value)]))

(define-syntax (store-wsdl stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-wsdl-table key value)]))

(define-syntax (make-attribute-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol xs:attribute) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-attribute-table r ...) 'x e)]))

(define-syntax (make-element-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol xs:element) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-element-table r ...) 'x e)]))

(define-syntax (make-operation-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol wsdl:operation) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-operation-table r ...) 'x e)]))

(define-syntax (make-part-table stx)
  (syntax-parse stx
    [(_)                #'(let ([t : (HashTable Symbol wsdl:part) (hash)]) t)]
    [(_ (x:id e) r ...) #'(hash-set (make-part-table r ...) 'x e)]))
  

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
                      [a-schema-table   (hash)])
         e_i ...
         (xs:schema
          (a-namespace)
          (a-import-table)
          (a-schema-table)))]))

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
     #'(store-schema
        x
        (make-xs:type e_i ...))]))

(: xs:range (-> Real Real xs:simple-type))
(define (xs:range lo hi)
  (xs:simple-type
   (xs:restriction
    (if (and (exact-integer? lo)
             (exact-integer? hi))
        xs:integer
        xs:decimal)
    (list (xs:min-inclusive lo)
          (xs:max-inclusive hi)))))

(: xs:enum (-> String * xs:simple-type))
(define (xs:enum . s-list)
  (xs:simple-type
   (xs:restriction
    xs:string
    (map xs:enumeration s-list))))

(: make-xs:simple-type (-> (U (-> qname) xs:simple-type) xs:restriction-member * xs:simple-type))
(define (make-xs:simple-type head . tail)
  (cond
    [(xs:simple-type? head)
     head]
    [else
     (xs:simple-type (xs:restriction head tail))]))

(: make-xs:complex-type-member (-> (U xs:sequence xs:all xs:choice (-> qname)) xs:complex-type-member))
(define (make-xs:complex-type-member e)
  (cond
    [(xs:sequence? e) e]
    [(xs:all? e)      e]
    [(xs:choice? e)   e]
    [else             (xs:restriction e '())]))
  

(define-syntax (make-xs:type stx)
  (syntax-parse stx

    ;; complex type with body
    [(_ ((a_i:id t_i r_i:boolean) ...) e)
     #'(xs:complex-type
        (make-attribute-table (a_i (xs:attribute t_i r_i)) ...)
        (make-xs:complex-type-member (resolve e)))]

    ;; complex type no body
    [(_ ((a_i:id t_i r_i:boolean) ...))
     #'(xs:complex-type
        (make-attribute-table (a_i (xs:attribute t_i r_i)) ...)
        #f)]

    ;; simple type
    [(_ head e_i ...)
     #'(make-xs:simple-type head e_i ...)]))


                               

(define-syntax (make-xs:all stx)
  (syntax-parse stx
    [(_ (x_i:id type_i hi_i lo_i) ...)
     #'(xs:all
        (make-element-table
         (x_i (xs:element (resolve type_i) hi_i lo_i)) ...))]))
         
(define-syntax (make-xs:sequence stx)
  (syntax-parse stx
    [(_ (x_i:id type_i hi_i lo_i) ...)
     #'(xs:sequence
        (make-element-table
         (x_i (xs:element (resolve type_i) hi_i lo_i)) ...))]))
         
(define-syntax (make-xs:choice stx)
  (syntax-parse stx
    [(_ hi lo (x_i:id type_i hi_i lo_i) ...)
     #'(xs:choice
        hi
        lo
        (make-element-table
         (x_i (xs:element (resolve type_i) hi_i lo_i)) ...))]))

(define-syntax (define-wsdl:definitions stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(define x : wsdl:definitions
         (make-wsdl:definitions
          e_i ...))]))

(define-syntax (make-wsdl:definitions stx)
  (syntax-parse stx
    [(_ e_i ...)
     #'(parameterize ([a-import-table (hash)]
                      [a-wsdl-table   (hash)])
         e_i ...
         (wsdl:definitions
          (a-namespace)
          (a-import-table)
          (a-wsdl-table)))]))
                 
(define-syntax (define-wsdl:port-type stx)
  (syntax-parse stx
    [(_ x:id r ...)
     #'(store-wsdl x (make-wsdl:port-type r ...))]))

(define-syntax (make-wsdl:port-type stx)
  (syntax-parse stx
    [(_ r ...)
     #'(wsdl:port-type (make-operation-table r ...))]))

(define-syntax (define-wsdl:message stx)
  (syntax-parse stx
    [(_ x:id r ...)
     #'(store-wsdl x (make-wsdl:message r ...))]))

(define-syntax (make-wsdl:message stx)
  (syntax-parse stx
    [(_ (x:id n) ...)
     #'(wsdl:message (make-part-table (x (wsdl:part n)) ...))]))

(define-syntax (make-wsdl:operation stx)
  (syntax-parse stx
    [(_ a)     #'(wsdl:operation (resolve a) #f #f)]
    [(_ a b)   #'(wsdl:operation (resolve a) (resolve b) #f)]
    [(_ a b c) #'(wsdl:operation (resolve a) (resolve b) (resolve c))]))











;; display forms

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

(define-syntax (display-wsdl:definitions stx)
  (syntax-parse stx
    ([_ e]
     #'(display-xexpr (wsdl->xexpr e)))))







