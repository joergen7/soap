#lang typed/racket/base

(require
 (only-in racket/match
          define/match))

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

(: wsdl:definitions-member<?
   (-> wsdl:definitions-member
       wsdl:definitions-member
       Boolean))
(define (wsdl:definitions-member<? a b)

  (: name (-> wsdl:definitions-member Symbol))
  (define/match (name x)
    [((wsdl:message name _part-set))
     name]
    [((wsdl:port-type name _operation-set))
     name])

  (symbol<? (name a) (name b)))
     
  

(provide
 (struct-out wsdl:definitions)
 wsdl:definitions-member
 wsdl:definitions-member?
 wsdl:definitions-member<?)


;; wsdl:message
;;------------------------------------------------------------

(struct wsdl:message
  ([name     : Symbol]
   [part-set : (Setof wsdl:part)])
  #:transparent)

(struct wsdl:part
  ([name : Symbol]
   [type : Symbol]) ; (U xs:qname xs:simple-type xs:complex-type)
  #:transparent)

(: wsdl:part<?
   (-> wsdl:part
       wsdl:part
       Boolean))
(define (wsdl:part<? a b)
  (symbol<? (wsdl:part-name a) (wsdl:part-name b)))

(provide
 (struct-out wsdl:message)
 (struct-out wsdl:part)
 wsdl:part<?)


;; wsdl:port-type
;;------------------------------------------------------------

(struct wsdl:port-type
  ([name          : Symbol]
   [operation-set : (Setof wsdl:operation)])
  #:transparent)

(struct wsdl:operation
  ([name   : Symbol]
   [input  : (U #f Symbol)]  ; wsdl:message
   [output : (U #f Symbol)]  ; wsdl:message
   [fault  : (U #f Symbol)]) ; wsdl:message
  #:transparent)

(: wsdl:operation<?
   (-> wsdl:operation
       wsdl:operation
       Boolean))
(define (wsdl:operation<? a b)
  (symbol<? (wsdl:operation-name a) (wsdl:operation-name b)))

(provide
 (struct-out wsdl:port-type)
 (struct-out wsdl:operation)
 wsdl:operation<?)


;; wsdl:binding
;;------------------------------------------------------------

(struct wsdl:binding
  ([name : Symbol]
   [type : Symbol]) ; wsdl:port-type
  #:transparent)
