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
          XExpr
          xexpr->string
          display-xml
          document
          prolog
          xexpr->xml
          element?
          empty-tag-shorthand)
 (only-in racket/set
          set
          set-member?)
 "xml-aux.rkt"
 "xml-schema.rkt"
 "xml-schema-validate.rkt"
 "xml-wsdl.rkt"
 "xml-wsdl-validate.rkt")

(provide
 #%top-interaction
 #%app
 #%top
 #%datum
 #%module-begin
 provide
 with-output-file
 in-namespace
 unbounded
 xs
 tns
 input
 output
 fault
 (rename-out
  [display-xs:schema        display-schema]
  [display-wsdl:definitions display-service]
  [define-xs:schema         define-schema]
  [define-xs:element        define-element]
  [define-xs:type           define-type]
  [define-xs:import         import]
  [make-xs:all              all]
  [make-xs:choice           choice]
  [xs:min-inclusive         min-inclusive]
  [xs:min-exclusive         min-exclusive]
  [xs:max-inclusive         max-inclusive]
  [xs:max-exclusive         max-exclusive]
  [xs:length                length]
  [xs:min-length            min-length]
  [xs:max-length            max-length]
  [make-xs:range            range]
  [make-xs:enum             enum]
  [make-xs:pattern          pattern]
  [define-wsdl:definitions  define-service]
  [define-wsdl:port-type    define-interface]
  [define-wsdl:message      define-message]
  [make-qname               :]
  [wsdl:part                part]
  ))


;; reader module

(module reader syntax/module-reader
  soap)


;; parameters

(define a-namespace : (Parameterof String)
  (make-parameter "urn:default"))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ s:str)
     #'(a-namespace s)]))

(: hash-set-1 (All (a b) (-> (HashTable a b) a b (HashTable a b))))
(define (hash-set-1 ht key v)
  (when (hash-has-key? ht key)
    (raise-user-error (format "name ~a already in use" key)))
  (when (set-member? reserved-set key)
    (raise-user-error (format "name ~a reserved" key)))
  (hash-set ht key v))

(define a-import-table : (Parameterof (HashTable Symbol (U xs:import xs:schema)))
  (let ([t0 : (HashTable Symbol (U xs:import xs:schema))
            (hash)])
    (make-parameter t0)))

(: store-import (-> Symbol (U xs:import xs:schema) Void))
(define (store-import x v)
  (a-import-table (hash-set-1 (a-import-table) x v)))

(define a-schema-table : (Parameterof (HashTable Symbol xs:schema-member))
  (let ([t0 : (HashTable Symbol xs:schema-member)
            (hash)])
    (make-parameter t0)))

(: store-schema (-> Symbol xs:schema-member Void))
(define (store-schema x v)
  (a-schema-table (hash-set-1 (a-schema-table) x v)))

(define a-wsdl-table : (Parameterof (HashTable Symbol wsdl:definitions-member))
  (let ([t0 : (HashTable Symbol wsdl:definitions-member)
            (hash)])
    (make-parameter t0)))

(: store-wsdl (-> Symbol wsdl:definitions-member Void))
(define (store-wsdl x v)
  (a-wsdl-table (hash-set-1 (a-wsdl-table) x v)))

(define a-operation-table : (Parameterof (HashTable Symbol (-> qname)))
  (let ([t0 : (HashTable Symbol (-> qname))
            (hash)])
    (make-parameter t0)))

(: store-operation (-> Symbol (-> qname) Void))
(define (store-operation x v)
  (a-operation-table (hash-set-1 (a-operation-table) x v)))
   


;; resolve

(define reserved-set : (Setof Symbol)
  (set 'boolean
       'date
       'dateTime
       'decimal
       'integer
       'nonNegativeInteger
       'NMTOKEN
       'string
       'token
       'unsignedShort))

(: resolve-qname (-> Symbol (-> qname)))
(define (resolve-qname name)
  (if (set-member? reserved-set name)
      (lambda () (qname (xs-prefix) name))
      (lambda () (qname (tns-prefix) name))))

(define-syntax (resolve stx)
  (syntax-parse stx
    [(_ x:id) #'(resolve-qname 'x)]
    [(_ x)    #'x]))


;; table constructors

(define-syntax (make-attribute-table stx)
  (syntax-parse stx

    [(_)
     #'(let ([t : (HashTable Symbol xs:attribute) (hash)]) t)]

    [(_ (x:id e) r ...)
     #'(hash-set-1 (make-attribute-table r ...) 'x (assert e xs:attribute?))]))

(define-syntax (make-element-table stx)
  (syntax-parse stx

    [(_)
     #'(let ([t : (HashTable Symbol xs:element) (hash)]) t)]
    
    [(_ (x:id e) r ...)
     #'(hash-set-1 (make-element-table r ...) 'x (assert e xs:element?))]))

(define-syntax (make-operation-table stx)
  (syntax-parse stx

    [(_)
     #'(let ([t : (HashTable Symbol wsdl:operation) (hash)]) t)]

    [(_ (x:id e) r ...)
     #'(hash-set-1 (make-operation-table r ...) 'x (assert e wsdl:operation?))]))

(define-syntax (make-part-table stx)
  (syntax-parse stx

    [(_)
     #'(let ([t : (HashTable Symbol wsdl:part) (hash)]) t)]

    [(_ (x:id e) r ...)
     #'(hash-set-1 (make-part-table r ...) 'x (assert e wsdl:part?))]))

;; language extensions

(define-syntax (define-xs:schema stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(define x : xs:schema
         (make-xs:schema
          (store-import
           (xs-prefix)
           (xs:import
            "http://www.w3.org/2001/XMLSchema"
            reserved-set
            (set)))
          e_i ...))]))

(define-syntax (make-xs:schema stx)
  (syntax-parse stx
    [(_ e_i ...)
     #'(parameterize ([a-import-table (hash)]
                      [a-schema-table (hash)])
         e_i ...
         (validate-xs:schema
          (xs:schema
           (a-namespace)
           (a-import-table)
           (a-schema-table))))]))

(define-syntax (define-xs:import stx)
  (syntax-parse stx
    
    ;; local import
    [(_ x:id a:id)
     #'(store-import 'x a)]

    ;; foreign import
    [(_ x:id e ...)
     #'(store-import 'x (make-xs:import e ...))]))


(define-syntax (make-xs:import stx)
  (syntax-parse stx
    
    ;; foreign import without declarations
    [(_ s:str)
     #'(make-xs:import s () ())]
    
    ;; Foreign import with declarations
    [(_ s:str (s_i:id ...) (c_i:id ...))
     #'(xs:import s (set 's_i ...) (set 'c_i ...))]))

(define-syntax (define-xs:element stx)
  (syntax-parse stx
    [(_ x:id y:id)
     #'(store-schema
        'x
        (make-xs:element (resolve y)))]))

(: make-xs:element (-> (-> qname) xs:element))
(define (make-xs:element qn)
  (xs:element qn 1 1))
        

(define-syntax (define-xs:type stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(store-schema
        'x
        (make-xs:type e_i ...))]))

(: make-xs:range (-> Real Real xs:simple-type))
(define (make-xs:range lo hi)
  (when (> lo hi)
    (raise-user-error "invalid range: [~a, ~a]" lo hi))
  (xs:simple-type
   (xs:restriction
    (if (and (exact-integer? lo)
             (exact-integer? hi))
        xs:integer
        xs:decimal)
    (list (xs:min-inclusive lo)
          (xs:max-inclusive hi)))))

(: make-xs:enum (-> String * xs:simple-type))
(define (make-xs:enum . s-list)
  (xs:simple-type
   (xs:restriction
    xs:string
    (map xs:enumeration s-list))))

(: make-xs:pattern (-> String xs:simple-type))
(define (make-xs:pattern p)
  (xs:simple-type
   (xs:restriction
    xs:string
    (list (xs:pattern p)))))

(: make-xs:simple-type (-> (U (-> qname) xs:simple-type) xs:restriction-member * xs:simple-type))
(define (make-xs:simple-type head . tail)
  (cond
    [(xs:simple-type? head)
     head]
    [else
     (xs:simple-type (xs:restriction head tail))]))

(: make-xs:complex-type-member (-> (U xs:all xs:choice (-> qname)) xs:complex-type-member))
(define (make-xs:complex-type-member e)
  (cond
    [(xs:all? e)      e]
    [(xs:choice? e)   e]
    [else             (xs:restriction e '())]))
  

(define-syntax (make-xs:type stx)
  (syntax-parse stx

    ;; complex type with body
    [(_ ((a_i:id t_i r_i:boolean) ...) e)
     #'(xs:complex-type
        (make-attribute-table (a_i (xs:attribute (resolve t_i) r_i)) ...)
        (make-xs:complex-type-member (resolve e)))]

    ;; complex type no body
    [(_ ((a_i:id t_i r_i:boolean) ...))
     #'(xs:complex-type
        (make-attribute-table (a_i (xs:attribute (resolve t_i) r_i)) ...)
        #f)]

    ;; simple type
    [(_ head e_i ...)
     #'(make-xs:simple-type (resolve head) e_i ...)]))


                               

(define-syntax (make-xs:all stx)
  (syntax-parse stx
    [(_ (x_i:id type_i hi_i lo_i) ...)
     #'(xs:all
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
         (validate-wsdl:definitions
          (wsdl:definitions
           (a-namespace)
           (a-import-table)
           (a-wsdl-table))))]))
                 
(define-syntax (define-wsdl:port-type stx)
  (syntax-parse stx
    [(_ x:id r ...)
     #'(store-wsdl 'x (make-wsdl:port-type r ...))]))

(define-syntax (make-wsdl:port-type stx)
  (syntax-parse stx
    [(_ [x:id e ...] ...)
     #'(wsdl:port-type (make-operation-table [x (make-wsdl:operation e ...)] ...))]))

(define-syntax (define-wsdl:message stx)
  (syntax-parse stx
    [(_ x:id r ...)
     #'(store-wsdl 'x (make-wsdl:message r ...))]))

(define-syntax (make-wsdl:message stx)
  (syntax-parse stx
    [(_ (x:id n) ...)
     #'(wsdl:message (make-part-table (x (wsdl:part n)) ...))]))


(define-syntax (input stx)
  (syntax-parse stx
    [(_ z)
     #'(store-operation 'input (resolve z))]))

(define-syntax (output stx)
  (syntax-parse stx
    [(_ z)
     #'(store-operation 'output (resolve z))]))

(define-syntax (fault stx)
  (syntax-parse stx
    [(_ z)
     #'(store-operation 'fault (resolve z))]))

(define-syntax (make-wsdl:operation stx)
  (syntax-parse stx
    [(_ e_i ...)
     #'(parameterize ([a-operation-table (hash)])
         e_i ...
         (wsdl:operation
          (hash-ref (a-operation-table) 'input (lambda () #f))
          (hash-ref (a-operation-table) 'output (lambda () #f))
          (hash-ref (a-operation-table) 'fault (lambda () #f))))]))


;; display forms

(: display-xexpr (-> XExpr Void))
(define (display-xexpr e)
  (parameterize ([empty-tag-shorthand 'always])
    (display-xml
     (document (prolog '() #f '())
               (assert (xexpr->xml e) element?)
               '()))))

(: display-xs:schema (-> xs:schema Void))
(define (display-xs:schema e)
  (display-xexpr (xs->xexpr e)))

(: display-wsdl:definitions (-> wsdl:definitions Void))
(define (display-wsdl:definitions e)
  (display-xexpr (wsdl->xexpr e)))

(define-syntax (with-output-file stx)
  (syntax-parse stx
    [(_ path:str e ...)
     #'(call-with-output-file
         path
         (lambda ([out : Output-Port])
           (parameterize ([current-output-port out])
             e ...))
         #:mode   'text
         #:exists 'replace)]))





