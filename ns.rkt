#lang typed/racket/base


(require
 (for-syntax
  (only-in syntax/parse
           syntax-parse
           id)
  (only-in racket/base
           syntax))
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
  
(define xml:prefix-table : (Immutable-HashTable Symbol String)
  (hash xs:prefix   "http://www.w3.org/2001/XMLSchema"
        wsdl:prefix "http://schemas.xmlsoap.org/wsdl"
        soap:prefix "http://schemas.xmlsoap.org/wsdl/soap"))

(provide
 xs:prefix
 wsdl:prefix
 soap:prefix
 xml:prefix-table)


;; xs
;;------------------------------------------------------------

(define xs:simple-provide-set : (Setof Symbol)
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

(: xs:make-qname (-> Symbol xs:qname))
(define (xs:make-qname ref)
  (xs:qname
   (xs:import
    xs:prefix
    (hash-ref xml:prefix-table xs:prefix)
    xs:simple-provide-set
    (set))
   ref))

(define-syntax (xs stx)
  (syntax-parse stx
      [(_ x:id)
       #'(xs:make-qname 'x)]))

(provide xs)


;; wsdl
;;------------------------------------------------------------

(: wsdl:make-qname (-> Symbol xs:qname))
(define (wsdl:make-qname ref)

  (xs:qname
   (xs:import
    wsdl:prefix
    (hash-ref xml:prefix-table wsdl:prefix)
    (set)
    (set))
   ref))

(define-syntax (wsdl stx)
  (syntax-parse stx
      [(_ x:id)
       #'(wsdl:make-qname 'x)]))

(provide wsdl)


;; soap
;;------------------------------------------------------------

(: soap:make-qname (-> Symbol xs:qname))
(define (soap:make-qname ref)

  (xs:qname
   (xs:import
    soap:prefix
    (hash-ref xml:prefix-table soap:prefix)
    (set)
    (set))
   ref))

(define-syntax (soap stx)
  (syntax-parse stx
      [(_ x:id)
       #'(soap:make-qname 'x)]))

(provide soap)

