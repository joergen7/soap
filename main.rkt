#lang racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           boolean
           id
           str))
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
 define-schema
 import
 define-type
 unbounded
 all
 sequence
 choice
 (rename-out
  [xs:min-inclusive min-inclusive]
  [xs:min-exclusive min-exclusive]
  [xs:max-inclusive max-inclusive]
  [xs:max-exclusive max-exclusive]
  [xs:pattern       pattern]
  [xs:length        length]
  [xs:min-length    min-length]
  [xs:max-length    max-length]))


;; reader module

(module reader syntax/module-reader
  soap)


;; parameters

(define a-namespace
  (make-parameter "urn:default"))

(define a-import-table
  (make-parameter (hash)))

(define a-schema-table
  (make-parameter (hash)))

(define a-type-table
  (make-parameter (hash)))


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

(define-syntax (store-schema stx)
  (syntax-parse stx
    [(_ key:id value)
     #'(store a-schema-table key value)]))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ s:str e_i ...) #'(parameterize ([a-namespace s]) e_i ...)]))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ x:id e_i ...)
     #'(store-schema
        x
        (parameterize ([a-import-table (hash)])
          e_i ...))]))

(define-syntax (import stx)
  (syntax-parse stx
    
    ;; foreign import without declarations
    [(_ x:id s:str)
     #'(import x s ())]
    
    ;; Foreign import with declarations
    [(import x:id s:str (a_i:id ...))
     #'(store-import
        x
        (xs:import s (set 'a_i ...)))]

    ;; local import
    [(import x:id a:id)
     #'(store-import
        x
        (hash-ref (a-schema-table) 'a))]))

(define-syntax (define-type stx)
  (syntax-parse stx
    #:datum-literals (xs enum)
    
    ;; simple type with base type in target namespace
    [(_ x:id base:id e_i ...)
     #'(store-type
        x
        (xs:simple-type (tns base) (list e_i ...)))]

    ;; simple type with base type in schema namespace
    [(_ x:id (xs base:id) e_i ...)
     #'(store-type
        x
        (xs:simple-type (xs base) (list e_i ...)))]

    ;; enum
    [(_ x:id (enum s_i:str ...))
     #'(store-type
        x
        (xs:simple-type
         xs:string
         (list (xs:enumeration s_i) ...)))]
    

    [(_ x:id ([a_i:id t_i:id r_i:boolean] ...) e)
     #'(store-type
        x
        (xs:complex-type
         (apply hash (append (list 'a_i (xs:attribute (tns t_i) r_i)) ...))
         e))]

    [(_ x:id ([a_i:id (xs t_i:id) r_i:boolean] ...) e)
     #'(store-type
        x
       (xs:complex-type
        (apply hash (append (list 'a_i (xs:attribute (xs t_i) r_i)) ...))
        e))]))


(define unbounded
  #f)

(define-syntax (all stx)
  (syntax-parse stx
    [(_ (x_i:id t_i:id lo_i hi_i) ...)
     #'(xs:all
        (apply (append (list 'x_i (xs:element (tns t_i) lo_i hi_i)) ...)))]))
         
(define-syntax (sequence stx)
  (syntax-parse stx
    [(_ (x_i:id t_i:id lo_i hi_i) ...)
     #'(xs:sequence
        (apply (append (list 'x_i (xs:element (tns t_i) lo_i hi_i)) ...)))]))
         
(define-syntax (choice stx)
  (syntax-parse stx
    [(_ lo hi (x_i:id t_i:id lo_i hi_i) ...)
     #'(xs:choice
        lo
        hi
        (apply (append (list 'x_i (xs:element (tns t_i) lo_i hi_i)) ...)))]))
         

;; TODO
;; - proper error message on hash-ref flunk
;; - no redefining schemas
;; - no name redefining prefixes on import








