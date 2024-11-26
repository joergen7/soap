#lang racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           id))
 "ns.rkt")

(provide
 xs
 wsdl
 soap)

(define-syntax (xs stx)
  (syntax-parse stx
      [(_ x:id)
       #'(xs:make-qname 'x)]))

(define-syntax (wsdl stx)
  (syntax-parse stx
      [(_ x:id)
       #'(wsdl:make-qname 'x)]))

(define-syntax (soap stx)
  (syntax-parse stx
      [(_ x:id)
       #'(soap:make-qname 'x)]))

