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
          set-union
          set-add)
 "xs.rkt"
 "xs-format.rkt"
 "wsdl.rkt"
 "wsdl-format.rkt"
 "ns-form.rkt"
 "display.rkt"
 "tns.rkt")

(provide
 #%top-interaction
 #%datum
 #%top
 #%app
 provide)



;; reader module
;;------------------------------------------------------------

(module reader syntax/module-reader
  soap)


;;============================================================
;; Module Context
;;============================================================

;; Shortcuts
;;------------------------------------------------------------

(define _unbounded
  #f)

(define _required
  #t)

(define _optional
  #f)

(define _string
  'string)

(define _NMTOKEN
  'NMTOKEN)

(define _date
  'date)

(define _dateTime
  'dateTime)

(define _decimal
  'decimal)

(define _integer
  'integer)

(define _nonNegativeInteger
  'nonNegativeInteger)

(define _unsignedShort
  'unsignedShort)

(provide
 (rename-out
  [_unbounded          unbounded]
  [_required           required]
  [_optional           optional]
  [_string             string]
  [_NMTOKEN            NMTOKEN]
  [_date               date]
  [_dateTime           dateTime]
  [_decimal            decimal]
  [_integer            integer]
  [_nonNegativeInteger nonNegativeInteger]
  [_unsignedShort      unsignedShort]))

;; Context Table
;;------------------------------------------------------------

(define a-ctx
  (make-parameter (hash)))

(define a-import-set
  (make-parameter (set)))

;; TODO: remove dis
(provide a-ctx a-import-set)

(define (ctx-format s)
  (xml:type->string (hash-ref (a-ctx) s)))

(define-syntax (ctx-value stx)
  (syntax-parse stx
    
    [(_ x:id e)
     #'(if (hash-has-key? (a-ctx) 'x)
           (raise-argument-error 'ctx-def "unused variable name" 'x)
           (begin
             (a-ctx (hash-set (a-ctx) 'x e))
             'x))]))

(define-syntax (ctx-define stx)
  (syntax-parse stx
    [(_ x:id form)
     #'(define x (ctx-value x form))]))

(define a-ctx0
  (hash
   'string             (xs string)
   'NMTOKEN            (xs NMTOKEN)
   'date               (xs date)
   'dateTime           (xs dateTime)
   'decimal            (xs decimal)
   'integer            (xs integer)
   'nonNegativeInteger (xs nonNegativeInteger)
   'unsignedShort      (xs unsignedShort)))

(define-syntax (_#%module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #'(#%module-begin (parameterize ([a-ctx a-ctx0]) form ...))]))

(provide
 (rename-out
  [_#%module-begin #%module-begin]))


;;============================================================
;; Namespace
;;============================================================

(define a-namespace
  (make-parameter "urn:default"))

(define-syntax (in-namespace stx)
  (syntax-parse stx
    [(_ ns:str)
     #'(a-namespace ns)]))

(provide in-namespace)

;;============================================================
;; Formatting
;;============================================================

(define (get-body pred)
  (for/fold ([result (set)])
            ([m (in-list (hash-values (a-ctx)))])
    (if (pred m)
        (set-add result m)
        result)))

(define-syntax (_display-schema stx)
  (syntax-parse stx
    [(_ name:id)
     #'(display-schema
        (xs:schema
         'name
         (a-namespace)
         (a-import-set)
         (get-body xs:schema-member?))
        ctx-format)]))

(define-syntax (_display-service stx)
  (syntax-parse stx
    [(_ name:id)
     #'(display-service
        (wsdl:definitions
         'name
         (a-namespace)
         (get-body wsdl:definitions-member?))
        ctx-format)]))

(provide
 (rename-out
  [_display-schema  display-schema]
  [_display-service display-service]))

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


;; import
;;------------------------------------------------------------

(define-syntax (import stx)
  (syntax-parse stx
    [(_ prefix:id ns:str (x:id ...) (y:id ...))
     #'(define-values (x ... y ...)
         (begin
           (a-import-set
            (set-add
             (a-import-set)
             (xs:import 'prefix ns (set 'x ...) (set 'y ...))))
           (values
            (ctx-value
             x
             (xs:qname 'prefix 'x)) ...
            (ctx-value
             y
             (xs:qname 'prefix 'y)) ...)))]))

(provide import)

;; define-type
;;------------------------------------------------------------

(define-syntax (define-type stx)
  (syntax-parse stx
    
    ;; complex type - no body
    [(_ name:id ([name_i type_i required_i] ...))
     #'(ctx-define
        name
        (xs:complex-type
         'name
         (set (make-xs:attribute name_i type_i required_i) ...)
         #f))]

    ;; complex type - with body
    [(_ name:id ([name_i type_i required_i] ...) body)
     #'(ctx-define
        name
        (xs:complex-type
         'name
         (set (make-xs:attribute name_i type_i required_i) ...)
         body))]

    ;; simple type or qname
    [(_ name:id restriction)
     #'(ctx-define
        name
        (xs:simple-type
         'name
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
     #'(xs:restriction 'string (set (xs:pattern s)))]))

(provide pattern)

;; range
;;------------------------------------------------------------

(define-syntax (range stx)
  (syntax-parse stx
    [(_ lo hi)
     #'(if (> lo hi)
           (raise-argument-error 'range "valid range" (list lo hi))
           (xs:restriction
            (if (and (exact-integer? lo)
                     (exact-integer? hi))
                'integer
                'decimal)
            (set (xs:min-inclusive lo)
                 (xs:max-inclusive hi))))]))

(provide range)

;; enum
;;------------------------------------------------------------

(define-syntax (enum stx)
  (syntax-parse stx
    [(_ s ...)
     #'(xs:restriction
        'string
        (set (xs:enumeration s) ...))]))

(provide enum)


;;============================================================
;; Service Language
;;============================================================

;; define-message
;;------------------------------------------------------------

(define-syntax (define-message stx)
  (syntax-parse stx
    [(_ name:id [part-name:id part-type] ...)
     #'(ctx-define
        name
        (wsdl:message 'name (set (wsdl:part 'part-name part-type) ...)))]))

(provide define-message)

;; define-interface
;;------------------------------------------------------------

(define-syntax (union stx)
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
         (union z_i ...))]))
  
(define a-output
  (make-parameter #f))

(define-syntax (with-output stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-output x])
         (union z_i ...))]))
  
(define a-fault
  (make-parameter #f))

(define-syntax (with-fault stx)
  (syntax-parse stx
    [(_ x:id z_i ...)
     #'(parameterize ([a-fault x])
         (union z_i ...))]))
  
(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id op_i ...)
     #'(ctx-define
        name
        (wsdl:port-type 'name (union op_i ...)))]))

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
