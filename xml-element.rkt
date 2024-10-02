#lang typed/racket/base

(require
  (for-syntax
   (only-in racket/base
            syntax)
   (only-in syntax/parse
            syntax-parse
            id))
  typed/xml
  "xml-qname.rkt")

(provide
 with-prefix
 clear-prefix
 make-xml-element)

(define prefix-context : (Parameterof (Listof (Pairof Symbol String)))
  (make-parameter '()))

(: make-xml-element (-> qname (Listof (Pairof Symbol String)) (Listof XExpr) XExpr))
(define (make-xml-element name att-list body)

  (: proc (-> (Pairof Symbol String) (List Symbol String)))
  (define (proc p)
    (list (car p) (cdr p)))

  (define z : (Listof (List Symbol String))
    (append (map proc (append (prefix-context) att-list))))

  (define a : (Pairof (Listof (List Symbol String)) (Listof XExpr))
    (cons z body))

  (define result : XExpr
    (cons (qname->symbol name) a))

  result)

;; Language extensions

(define-syntax (with-prefix stx)
  (syntax-parse stx
    
    [(_ ([a:id b] [a_i:id b_i] ...) x ...)
     #'(parameterize ([prefix-context (cons (cons (qname->symbol (qname 'xmlns 'a)) b) (prefix-context))])
         (with-prefix ([a_i b_i] ...) x ...))]

    [(_ () x ...)
     #'(begin x ...)]))

(define-syntax (clear-prefix stx)
  (syntax-parse stx
    [(_ x ...)
     #'(parameterize ([prefix-context '()]) x ...)]))

