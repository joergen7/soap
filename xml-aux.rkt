#lang typed/racket/base

(require
  (for-syntax
   (only-in racket/base
            syntax)
   (only-in syntax/parse
            syntax-parse
            id))
  (only-in typed/xml
           XExpr))

(provide (struct-out qname)
         qname->string
         qname->symbol
         make-xml-element
         xs-prefix
         xs
         tns-prefix
         tns
         wsdl-prefix
         wsdl
         types-prefix
         types)

(struct qname
  ([prefix : Symbol]
   [name   : Symbol]))

(: qname->string (-> qname String))
(define (qname->string qn)
  (format "~a:~a" (qname-prefix qn) (qname-name qn)))

(: qname->symbol (-> qname Symbol))
(define (qname->symbol qn)
  (string->symbol (qname->string qn)))

(: make-xml-element (->* (qname)
                         (#:prefix-list (Listof (Pairof Symbol String))
                          #:att-list    (Listof (Pairof Symbol String))
                          #:name-value  (U #f Symbol)
                          #:body        (Listof XExpr))
                         XExpr))
(define (make-xml-element name
                          #:prefix-list (prefix-list '())
                          #:att-list    (att-list    '())
                          #:name-value  (name-value  #f)
                          #:body        (body        '()))

  (: proc-prefix (-> (Pairof Symbol String) (List Symbol String)))
  (define (proc-prefix p)
    (list (qname->symbol (qname 'xmlns (car p))) (cdr p)))

  (: proc-att (-> (Pairof Symbol String) (List Symbol String)))
  (define (proc-att p)
    (list (car p) (cdr p)))

  (define z : (Listof (List Symbol String))
    (append
     (if name-value
         (list (list 'name (symbol->string name-value)))
         '())
     (map proc-prefix prefix-list)
     (map proc-att att-list)))

  (define a : (Pairof (Listof (List Symbol String)) (Listof XExpr))
    (cons z body))

  (define result : XExpr
    (cons (qname->symbol name) a))

  result)

(define xs-prefix : (Parameterof Symbol)
  (make-parameter 'xs))

(define-syntax (xs stx)
  (syntax-parse stx
    [(_ x:id) #'(λ () (qname (xs-prefix) 'x))]))

(define tns-prefix : (Parameterof Symbol)
  (make-parameter 'tns))

(define-syntax (tns stx)
  (syntax-parse stx
    [(_ x:id) #'(λ () (qname (tns-prefix) 'x))]))

(define wsdl-prefix : (Parameterof Symbol)
  (make-parameter 'wsdl))

(define-syntax (wsdl stx)
  (syntax-parse stx
    [(_ x:id) #'(λ () (qname (wsdl-prefix) 'x))]))

(define types-prefix : (Parameterof Symbol)
  (make-parameter 'types))

(define-syntax (types stx)
  (syntax-parse stx
    [(_ x:id) #'(λ () (qname (types-prefix) 'x))]))