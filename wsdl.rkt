#lang typed/racket/base

(require
  "xs.rkt")

;; wsdl:definitions
;;------------------------------------------------------------

(struct wsdl:definitions
  ([name      : Symbol]
   [namespace : String]
   [body      : (Setof wsdl:definitions-member)])
  #:transparent)

(define-type wsdl:definitions-member
  (U wsdl:message
     wsdl:port-type))

(define-predicate wsdl:definitions-member?
  wsdl:definitions-member)

(provide
 (struct-out wsdl:definitions)
 wsdl:definitions-member
 wsdl:definitions-member?)


;; wsdl:message
;;------------------------------------------------------------

(struct wsdl:message
  ([name     : Symbol]
   [part-set : (Setof wsdl:part)])
  #:transparent)

(struct wsdl:part
  ([name : Symbol]
   [type : (U xs:qname xs:simple-type xs:complex-type)])
  #:transparent)

(provide
 (struct-out wsdl:message)
 (struct-out wsdl:part))


;; wsdl:port-type
;;------------------------------------------------------------

(struct wsdl:port-type
  ([name          : Symbol]
   [operation-set : (Setof wsdl:operation)])
  #:transparent)

(struct wsdl:operation
  ([name   : Symbol]
   [input  : (U #f wsdl:message)]
   [output : (U #f wsdl:message)]
   [fault  : (U #f wsdl:message)])
  #:transparent)

(provide
 (struct-out wsdl:port-type)
 (struct-out wsdl:operation))

