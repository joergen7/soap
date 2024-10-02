#lang typed/racket/base

(provide (struct-out qname)
         qname->string
         qname->symbol)

(struct qname
  ([prefix : Symbol]
   [name   : Symbol]))

(: qname->string (-> qname String))
(define (qname->string qn)
  (format "~a:~a" (qname-prefix qn) (qname-name qn)))

(: qname->symbol (-> qname Symbol))
(define (qname->symbol qn)
  (string->symbol (qname->string qn)))