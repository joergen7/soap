#lang typed/racket/base

(require
  (for-syntax
   (only-in racket/base
            syntax)
   (only-in syntax/parse
            syntax-parse
            id))
  racket/match
  typed/xml
  "xml-qname.rkt"
  "xml-element.rkt")

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

(struct xs:schema
  ([target-namespace : String]
   [body             : (Listof xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(struct xs:element
  ([name       : Symbol]
   [type       : (-> qname)]
   [default    : (U #f String)]
   [min-occurs : Nonnegative-Integer]
   [max-occurs : Nonnegative-Integer]))

;; base types

(define xs:string
  (xs string))

(define xs:decimal
  (xs decimal))

(define xs:integer
  (xs integer))

(define xs:boolean
  (xs boolean))

(define xs:date
  (xs date))

(define xs:time
  (xs time))

;; simple-type

(struct xs:simple-type
  ([name : Symbol]
   [body : (Listof xs:simple-type-member)]))

(define-type xs:simple-type-member
  (U xs:restriction))

(struct xs:restriction
  ([base : (-> qname)]
   [body : xs:restriction-member]))

(define-type xs:restriction-member
  (U xs:min-inclusive
     xs:min-exclusive
     xs:max-inclusive
     xs:max-exclusive
     xs:enumeration
     xs:pattern
     xs:length
     xs:min-length
     xs:max-length))

(struct xs:min-inclusive
  ([value : Real]))

(struct xs:min-exclusive
  ([value : Real]))

(struct xs:max-inclusive
  ([value : Real]))

(struct xs:max-exclusive
  ([value : Real]))

(struct xs:enumeration
  ([value : String]))

(struct xs:pattern
  ([value : String]))

(struct xs:length
  ([value : Nonnegative-Integer]))

(struct xs:min-length
  ([value : Nonnegative-Integer]))

(struct xs:max-length
  ([value : Nonnegative-Integer]))

;; complex type

(struct xs:complex-type
  ([name : Symbol]
   [body : (Listof xs:complex-type-member)]))

(define-type xs:complex-type-member
  (U xs:attribute
     xs:sequence
     xs:all
     xs:choice))

(struct xs:attribute
  ([name     : Symbol]
   [type     : (-> qname)]
   [default  : (U #f String)]
   [required : Boolean]))

(struct xs:sequence
  ([min-occurs : Nonnegative-Integer]
   [max-occurs : Nonnegative-Integer]
   [body       : (Listof xs:element)]))

(struct xs:all
  ([min-occurs : Nonnegative-Integer]
   [max-occurs : Nonnegative-Integer]
   [body       : (Listof xs:element)]))

(struct xs:choice
  ([min-occurs : Nonnegative-Integer]
   [max-occurs : Nonnegative-Integer]
   [body : (Listof xs:element)]))


(: make-occur-list (-> Nonnegative-Integer Nonnegative-Integer (Listof (Pairof Symbol String))))
(define (make-occur-list min-occurs max-occurs)
   (let ([l1 : (Listof (Pairof Symbol String))
             (if (= min-occurs 1)
                 '()
                 (list (cons 'minOccurs (number->string min-occurs))))]
         [l2 : (Listof (Pairof Symbol String))
             (if (= max-occurs 1)
                 '()
                 (list (cons 'maxOccurs (number->string max-occurs))))])
     (append l1 l2)))

(: xs->xexpr (-> Any XExpr))
(define/match (xs->xexpr x)

  ;; xs:schema
  [((xs:schema target-namespace body))
   (with-prefix ([tns target-namespace]
                 [xs  "http://www.w3.org/2001/XMLSchema"])
     (make-xml-element
      ((xs schema))
      (list (cons 'targetNamespace target-namespace))
      (clear-prefix
       (map xs->xexpr body))))]

  ;; xs:element
  [((xs:element name type default min-occurs max-occurs))
   (let ([occur-list : (Listof (Pairof Symbol String))
                     (make-occur-list min-occurs max-occurs)]
         [default-list : (Listof (Pairof Symbol String))
           (if default
               (list (cons 'default default))
               '())]
         [name-list : (Listof (Pairof Symbol String))
                    (list (cons 'name (symbol->string name)))]
         [type-list : (Listof (Pairof Symbol String))
                    (list (cons 'type (qname->string (type))))])
   (make-xml-element
    ((xs element))
    (append
     name-list
     type-list
     default-list
     occur-list)
    '()))]

  ;; xs:simple-type

  ;; xs:restriction

  ;; xs:min-inclusive

  ;; xs:min-exclusive

  ;; xs:enumeration

  ;; xs:pattern

  ;; xs:length

  ;; xs:min-length

  ;; xs:max-length

  ;; xs:complex-type


  ;; xs:attribute
  [((xs:attribute name type default required))
   (let ([name-list : (Listof (Pairof Symbol String))
                    (list (cons 'name (symbol->string name)))]
         [type-list : (Listof (Pairof Symbol String))
                    (list (cons 'type (qname->string (type))))]
         [default-list : (Listof (Pairof Symbol String))
           (if default
               (list (cons 'default default))
               '())]
         [required-list : (Listof (Pairof Symbol String))
                        (if required
                            (list (cons 'use "required"))
                            '())])
     (make-xml-element
      ((xs attribute))
      (append
       name-list
       type-list
       default-list
       required-list)
      '()))]

  ;; xs:sequence
  [((xs:sequence min-occurs max-occurs body))
   (make-xml-element
    ((xs sequence))
    (make-occur-list min-occurs max-occurs)
    (clear-prefix
     (map xs->xexpr body)))]

  ;; xs:all
  [((xs:all min-occurs max-occurs body))
   (make-xml-element
    ((xs all))
    (make-occur-list min-occurs max-occurs)
    (clear-prefix
     (map xs->xexpr body)))]

;; xs:choice
  [((xs:choice min-occurs max-occurs body))
   (make-xml-element
    ((xs choice))
    (make-occur-list min-occurs max-occurs)
    (clear-prefix
     (map xs->xexpr body)))])

(module+ test

  (require typed/rackunit)
  
  (define a-element : xs:element
    (xs:element 'value xs:string #f 1 1))

  (define x-element : XExpr
    '(xs:element ((name "value") (type "xs:string"))))

  (check-equal? (xs->xexpr a-element)
                x-element)

  (check-equal? (xs->xexpr (xs:element 'blub (tns mytype) "1" 2 3))
                '(xs:element ((name      "blub")
                              (type      "tns:mytype")
                              (default   "1")
                              (minOccurs "2")
                              (maxOccurs "3"))))

  (let ([element (xs:element 'blub (tns mytype) "1" 2 3)])
    (check-equal? (parameterize ([xs-prefix 'xsd]
                                 [tns-prefix 'tanaspa])
                    (xs->xexpr element))
                  '(xsd:element ((name      "blub")
                                 (type      "tanaspa:mytype")
                                 (default   "1")
                                 (minOccurs "2")
                                 (maxOccurs "3")))))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace" (list a-element)))
                (list 'xs:schema '((xmlns:xs        "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns       "urn:target-namespace")
                                   (targetNamespace "urn:target-namespace"))
                      x-element))

  (check-equal? (xs->xexpr (xs:attribute 'prodid xs:string #f #f))
                '(xs:attribute ((name "prodid")
                                (type "xs:string"))))

  (check-equal? (xs->xexpr (xs:attribute 'prodid xs:string "blub" #t))
                '(xs:attribute ((name    "prodid")
                                (type    "xs:string")
                                (default "blub")
                                (use     "required"))))
  )
     

