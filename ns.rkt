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

(provide xs:make-qname)


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

(provide wsdl:make-qname)


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

(provide soap:make-qname)


;; xml:shorthand-list
;;------------------------------------------------------------

(define xml:shorthand-list : (Listof Symbol)
  (map
   xs:qname->symbol
   (list
    (xs:make-qname 'import)
    (xs:make-qname 'element)
    (xs:make-qname 'minInclusive)
    (xs:make-qname 'minExclusive)
    (xs:make-qname 'maxInclusive)
    (xs:make-qname 'maxExclusive)
    (xs:make-qname 'enumeration)
    (xs:make-qname 'pattern)
    (xs:make-qname 'length)
    (xs:make-qname 'minLength)
    (xs:make-qname 'maxLength)
    (xs:make-qname 'attribute)
    (wsdl:make-qname 'input)
    (wsdl:make-qname 'output)
    (wsdl:make-qname 'fault)
    (wsdl:make-qname 'part))))


(provide xml:shorthand-list)
