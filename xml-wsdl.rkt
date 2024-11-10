#lang typed/racket/base

(require
  "xml-schema.rkt")

;; wsdl:definitions
;;------------------------------------------------------------

(struct wsdl:definitions
  ([namespace : String]
   [body      : (Setof wsdl:definitions-member)]))

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
   [part-set : (Setof wsdl:part)]))

(struct wsdl:part
  ([name : Symbol]
   [type : xs:qname]))

(provide
 (struct-out wsdl:message)
 (struct-out wsdl:part))


;; wsdl:port-type
;;------------------------------------------------------------

(struct wsdl:port-type
  ([name          : Symbol]
   [operation-set : (Setof wsdl:operation)]))

(struct wsdl:operation
  ([name   : Symbol]
   [input  : (U #f wsdl:message)]
   [output : (U #f wsdl:message)]
   [fault  : (U #f wsdl:message)]))

(provide
 (struct-out wsdl:port-type)
 (struct-out wsdl:operation))

