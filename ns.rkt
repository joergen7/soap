#lang typed/racket/base


(require
 (only-in racket/set
          set)
 "xs.rkt")


;; prefix-table
;;------------------------------------------------------------

(define xs:prefix : Symbol
  'xs)

(define wsdl:prefix : Symbol
  'wsdl)

(define soap:prefix : Symbol
  'soap)
  
(provide
 xs:prefix
 wsdl:prefix
 soap:prefix)



;; xs
;;------------------------------------------------------------

(: xs:make-qname (-> Symbol xs:qname))
(define (xs:make-qname ref)
  (xs:qname xs:prefix ref))

(provide xs:make-qname)


;; wsdl
;;------------------------------------------------------------

(: wsdl:make-qname (-> Symbol xs:qname))
(define (wsdl:make-qname ref)
  (xs:qname wsdl:prefix ref))

(provide wsdl:make-qname)


;; soap
;;------------------------------------------------------------

(: soap:make-qname (-> Symbol xs:qname))
(define (soap:make-qname ref)
  (xs:qname soap:prefix ref))

(provide soap:make-qname)


