#lang typed/racket/base

(require
 (only-in typed/xml
          XExpr)
 (only-in racket/match
          define/match))

(: make-xml-element (->* (Symbol)
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
  (define/match (proc-prefix p)
    [((cons prefix namespace))
     (let ([s (string->symbol (format "xmlns:~a" prefix))])
       (list s namespace))])

  (: proc-att (-> (Pairof Symbol String) (List Symbol String)))
  (define/match (proc-att p)
    [((cons att-name att-value))
     (list att-name att-value)])

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
    (cons name a))

  result)

(provide make-xml-element)

(module+ test

  (require typed/rackunit)

  (check-equal? (make-xml-element 'blub)
                '(blub ()))

  (check-equal? (make-xml-element
                 'blub
                 #:body '((bla ())))
                '(blub () (bla ())))

  (check-equal? (make-xml-element
                 'blub
                 #:body '((bla ())
                          (foo ())))
                '(blub () (bla ()) (foo ())))

  (check-equal? (make-xml-element
                 'blub
                 #:att-list '((x . "1")))
                '(blub ((x "1"))))

  (check-equal? (make-xml-element
                 'blub
                 #:att-list '((x . "1")
                              (y . "2")))
                '(blub ((x "1") (y "2"))))

  (check-equal? (make-xml-element
                 'blub
                 #:prefix-list '((a . "http://a.org")))
                '(blub ((xmlns:a "http://a.org"))))
  
  (check-equal? (make-xml-element
                 'blub
                 #:prefix-list '((a . "http://a.org")
                                 (b . "http://b.org")))
                '(blub ((xmlns:a "http://a.org")
                        (xmlns:b "http://b.org"))))

  (check-equal? (make-xml-element
                 'blub
                 #:name-value 'test)
                '(blub ((name "test")))))
