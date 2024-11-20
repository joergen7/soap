#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           id
           str))
 (only-in racket/set
          set
          list->set)
 (only-in typed/xml
          empty-tag-shorthand
          display-xml
          document
          prolog
          xexpr->xml
          element?
          XExpr)
 "xs.rkt"
 "xs-format.rkt"
 "wsdl.rkt"
 "wsdl-format.rkt"
 "ns.rkt")

(provide
 #%module-begin
 #%top-interaction
 #%datum
 #%top
 #%app
 provide)



 ;; reader module
;;------------------------------------------------------------

(module reader syntax/module-reader
  soap)

;; in-namespace
;;------------------------------------------------------------

(define a-namespace : (Parameterof String)
  (make-parameter "urn:default"))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ ns:str)
     #'(a-namespace ns)]))

(provide in-namespace)

;;=============================================================
;; Display Forms
;;============================================================

(: display-xexpr (-> XExpr Void))
(define (display-xexpr e)
  (parameterize ([empty-tag-shorthand 'always])
    (display-xml
     (document (prolog '() #f '())
               (assert (xexpr->xml e) element?)
               '()))))

(: display-schema (-> xs:schema Void))
(define (display-schema e)
  (display-xexpr (xs:xs->xexpr e)))

(: display-service (-> wsdl:definitions Void))
(define (display-service e)
  (display-xexpr (wsdl:wsdl->xexpr e)))

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

(provide
 display-schema
 display-service
 with-output-file)

;;============================================================
;; Schema Language
;;============================================================

(define-syntax (make-xs:element stx)
  (syntax-parse stx
    [(_ name:id type lo hi)
     #'(xs:element 'name type lo hi)]))

(define-syntax (make-xs:attribute stx)
  (syntax-parse stx
    [(_ name:id type required)
     #'(xs:attribute 'name type required)]))


;; define-schema
;;------------------------------------------------------------

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name x ...)
     #'(define name : xs:schema
         (xs:schema 'name (a-namespace) (set x ...)))]))

(provide define-schema)

;; import
;;------------------------------------------------------------

(define-syntax (import stx)
  (syntax-parse stx
    [(_ prefix:id ns:str (x:id ...) (y:id ...))
     #'(define-values (x ... y ...)
         (values
          (xs:qname
           (xs:import 'prefix ns (set 'x ...) (set 'y ...))
           'x) ...
          (xs:qname
           (xs:import 'prefix ns (set 'x ...) (set 'y ...))
           'y) ...))]))

(provide import)

;; define-type
;;------------------------------------------------------------

(define-syntax (define-type stx)
  (syntax-parse stx
    ;; complex type - no body
    [(_ name:id ([name_i type_i required_i] ...))
     #'(define name : xs:complex-type
         (xs:complex-type 'name
                          (set (make-xs:attribute name_i type_i required_i) ...)
                          #f))]

    ;; complex type - with body
    [(_ name:id ([name_i type_i required_i] ...) body)
     #'(define name : xs:complex-type
         (xs:complex-type 'name
                          (set (make-xs:attribute name_i type_i required_i) ...)
                          body))]

    ;; simple type or qname
    [(_ name:id restriction)
     #'(define name : xs:simple-type
         (xs:simple-type 'name
                         restriction))]))


(provide define-type)

;; all
;;------------------------------------------------------------

(define-syntax (all stx)
  (syntax-parse stx
    [(_ [x ...] ...)
     #'(xs:all (set [make-xs:element x ...] ...))]))

(provide all)

;; choice
;;------------------------------------------------------------

(define-syntax (choice stx)
  (syntax-parse stx
    [(_ lo hi (x ...) ...)
     #'(xs:choice lo hi (set (make-xs:element x ...) ...))]))

(provide choice)

;; as
;;------------------------------------------------------------

(: as (-> (U xs:qname xs:simple-type)
          xs:restriction-member *
          xs:restriction))
(define (as base . x-list)
  (xs:restriction base (list->set x-list)))

(provide as)

;; pattern
;;------------------------------------------------------------

(: pattern (-> String xs:restriction))
(define (pattern p)
  (xs:restriction (xs string) (set (xs:pattern p))))

(provide pattern)

;; range
;;------------------------------------------------------------

(: range (-> Real
             Real
             xs:restriction))
(define (range lo hi)

  (when (> lo hi)
    (raise-argument-error 'range "invalid range" (list lo hi)))

  (xs:restriction
   (if (and (exact-integer? lo)
            (exact-integer? hi))
       (xs integer)
       (xs decimal))
       (set (xs:min-inclusive lo)
            (xs:max-inclusive hi))))

(provide range)

;; enum
;;------------------------------------------------------------

(: enum (-> String * xs:restriction))
(define (enum . s-list)
  (xs:restriction
   (xs string)
   (list->set
    (map xs:enumeration s-list))))

(provide enum)

;; Shortcuts
;;------------------------------------------------------------

(define unbounded : False
  #f)

(define required : Boolean
  #t)

(define optional : Boolean
  #f)

(define string : xs:qname
  (xs string))

(define date : xs:qname
  (xs date))

(define integer : xs:qname
  (xs integer))

(provide
 unbounded
 required
 optional
 string
 date
 integer)

;;============================================================
;; Service Language
;;============================================================

;; define-service
;;------------------------------------------------------------

(define-syntax (define-service stx)
  (syntax-parse stx
    [(_ name:id x ...)
     #'(define name : wsdl:definitions
         (wsdl:definitions 'name (a-namespace) (set x ...)))]))

(provide define-service)

;; define-message
;;------------------------------------------------------------

(define-syntax (define-message stx)
  (syntax-parse stx
    [(_ name:id [part-name:id part-type] ...)
     #'(define name : wsdl:message
         (wsdl:message 'name (set (wsdl:part 'part-name part-type) ...)))]))

(provide define-message)

;; define-interface
;;------------------------------------------------------------

(define a-input : (Parameterof (U False wsdl:message))
  (make-parameter #f))

(define-syntax (with-input stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-input x])
         z_i ...)]))
  
(define a-output : (Parameterof (U False wsdl:message))
  (make-parameter #f))

(define-syntax (with-output stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-output x])
         z_i ...)]))
  
(define a-fault : (Parameterof (U False wsdl:message))
  (make-parameter #f))

(define-syntax (with-fault stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-fault x])
         z_i ...)]))
  
(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id op_i ...)
     #'(define name : wsdl:port-type
         (wsdl:port-type
          'name
          (set
           op_i ...)))]))

(define-syntax (op stx)
  (syntax-parse stx
    [(_ name:id)
     #'(wsdl:operation 'name (a-input) (a-output) (a-fault))]))

(provide
 with-input
 with-output
 with-fault
 define-interface
 op)
