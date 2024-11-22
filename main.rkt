#lang racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           id
           str))
 (only-in racket/set
          list->set
          set
          set-union)
 "xs.rkt"
 "xs-format.rkt"
 "wsdl.rkt"
 "wsdl-format.rkt"
 "ns-forms.rkt"
 "display.rkt")

(provide
 #%top-interaction
 #%datum
 #%top
 #%app
 provide
 (all-from-out "display.rkt"))



;; reader module
;;------------------------------------------------------------

(module reader syntax/module-reader
  soap)



;;============================================================
;; Module Context
;;============================================================


(define-syntax (_#%module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #'(#%module-begin form ...)]))

(provide
 (rename-out
  [_#%module-begin #%module-begin]))



;; in-namespace
;;------------------------------------------------------------

(define a-namespace
  (make-parameter "urn:default"))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ ns:str)
     #'(a-namespace ns)]))

(provide in-namespace)

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
     #'(define name
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
     #'(define name
         (xs:complex-type 'name
                          (set (make-xs:attribute name_i type_i required_i) ...)
                          #f))]

    ;; complex type - with body
    [(_ name:id ([name_i type_i required_i] ...) body)
     #'(define name
         (xs:complex-type 'name
                          (set (make-xs:attribute name_i type_i required_i) ...)
                          body))]

    ;; simple type or qname
    [(_ name:id restriction)
     #'(define name
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

(define-syntax (as stx)
  (syntax-parse stx
    [(_ base x ...)
     #'(xs:restriction base (set x ...))]))

(provide as)

;; pattern
;;------------------------------------------------------------

(define-syntax (pattern stx)
  (syntax-parse stx
    [(_ s)
     #'(xs:restriction (xs string) (set (xs:pattern s)))]))

(provide pattern)

;; range
;;------------------------------------------------------------

(define-syntax (range stx)
  (syntax-parse stx
    [(_ lo hi)
     #'(if (> lo hi)
           (raise-argument-error 'range "invalid range" (list lo hi))
           (xs:restriction
            (if (and (exact-integer? lo)
                     (exact-integer? hi))
                (xs integer)
                (xs decimal))
            (set (xs:min-inclusive lo)
                 (xs:max-inclusive hi))))]))

(provide range)

;; enum
;;------------------------------------------------------------

(define-syntax (enum stx)
  (syntax-parse stx
    [(_ s ...)
     #'(xs:restriction
        (xs string)
        (set s ...))]))

(provide enum)

;; Shortcuts
;;------------------------------------------------------------


(define _unbounded
  #f)

(define _required
  #t)

(define _optional
  #f)

(define _string
  (xs string))

(define _date
  (xs date))

(define _integer
  (xs integer))

(define _NMTOKEN
  (xs NMTOKEN))

(define _boolean
  (xs boolean))

(define _nonNegativeInteger
  (xs nonNegativeInteger))

(define _dateTime
  (xs dateTime))

(define _unsignedShort
  (xs unsignedShort))

(provide
 (rename-out
  [_unbounded          unbounded]
  [_required           required]
  [_optional           optional]
  [_string             string]
  [_date               date]
  [_integer            integer]
  [_NMTOKEN            NMTOKEN]
  [_boolean            boolean]
  [_nonNegativeInteger nonNegativeInteger]
  [_dateTime           dateTime]
  [_unsignedShort     unsignedShort]))

;;============================================================
;; Service Language
;;============================================================

;; define-service
;;------------------------------------------------------------

(define-syntax (define-service stx)
  (syntax-parse stx
    [(_ name:id x ...)
     #'(define name
         (wsdl:definitions 'name (a-namespace) (set x ...)))]))

(provide define-service)

;; define-message
;;------------------------------------------------------------

(define-syntax (define-message stx)
  (syntax-parse stx
    [(_ name:id [part-name:id part-type] ...)
     #'(define name
         (wsdl:message 'name (set (wsdl:part 'part-name part-type) ...)))]))

(provide define-message)

;; define-interface
;;------------------------------------------------------------

(define-syntax (a-set-union stx)
  (syntax-parse stx
    [(_)
     #'(set)]
    [(_ st ...)
     #'(set-union st ...)]))


(define a-input
  (make-parameter #f))

(define-syntax (with-input stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-input x])
         (a-set-union z_i ...))]))
  
(define a-output
  (make-parameter #f))

(define-syntax (with-output stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-output x])
         (a-set-union z_i ...))]))
  
(define a-fault
  (make-parameter #f))

(define-syntax (with-fault stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-fault x])
         (a-set-union z_i ...))]))
  
(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id op_i ...)
     #'(define name
         (wsdl:port-type
          'name
          (a-set-union op_i ...)))]))

(define-syntax (op stx)
  (syntax-parse stx
    [(_ name:id)
     #'(set (wsdl:operation 'name (a-input) (a-output) (a-fault)))]))

(provide
 with-input
 with-output
 with-fault
 define-interface
 op)
