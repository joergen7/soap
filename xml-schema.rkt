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
   [ns-list          : (Listof (Pairof Symbol String))]
   [body             : (Listof xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(struct xs:element
  ([name       : Symbol]
   [type       : (-> qname)]
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
   [base : (-> qname)]
   [body : (Listof xs:restriction-member)]))

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
  [((xs:schema target-namespace ns-list body))
   (set-prefix ns-list
               (with-prefix ([tns target-namespace]
                             [xs  "http://www.w3.org/2001/XMLSchema"])
                 (make-xml-element
                  ((xs schema))
                  (list (cons 'targetNamespace target-namespace))
                  (clear-prefix
                   (map xs->xexpr body)))))]

  ;; xs:element
  [((xs:element name type min-occurs max-occurs))
   (let ([occur-list : (Listof (Pairof Symbol String))
                     (make-occur-list min-occurs max-occurs)]
         [name-list : (Listof (Pairof Symbol String))
                    (list (cons 'name (symbol->string name)))]
         [type-list : (Listof (Pairof Symbol String))
                    (list (cons 'type (qname->string (type))))])
   (make-xml-element
    ((xs element))
    (append
     name-list
     type-list
     occur-list)
    '()))]

  ;; xs:simple-type
  [((xs:simple-type name base body))
   (make-xml-element
    ((xs simpleType))
    (list (cons 'name (symbol->string name)))
    (clear-prefix
     (list
      (make-xml-element
       ((xs restriction))
       (list (cons 'base (qname->string (base))))
       (map xs->xexpr body)))))]

  ;; xs:min-inclusive
  [((xs:min-inclusive value))
   (make-xml-element
    ((xs minInclusive))
    (list (cons 'value (number->string value)))
    '())]
  
  ;; xs:min-exclusive
  [((xs:min-exclusive value))
   (make-xml-element
    ((xs minExclusive))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:max-inclusive
  [((xs:max-inclusive value))
   (make-xml-element
    ((xs maxInclusive))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:max-exclusive
  [((xs:max-exclusive value))
   (make-xml-element
    ((xs maxExclusive))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:enumeration
  [((xs:enumeration value))
   (make-xml-element
    ((xs enumeration))
    (list (cons 'value value))
    '())]

  ;; xs:pattern
  [((xs:pattern value))
   (make-xml-element
    ((xs pattern))
    (list (cons 'value value))
    '())]

  ;; xs:length
  [((xs:length value))
   (make-xml-element
    ((xs length))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:min-length
  [((xs:min-length value))
   (make-xml-element
    ((xs minLength))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:max-length
  [((xs:max-length value))
   (make-xml-element
    ((xs maxLength))
    (list (cons 'value (number->string value)))
    '())]

  ;; xs:complex-type
  [((xs:complex-type name body))
   (make-xml-element
    ((xs complex-type))
    (list (cons 'name (symbol->string name)))
    (clear-prefix
     (map xs->xexpr body)))]

  ;; xs:attribute
  [((xs:attribute name type required))
   (let ([name-list : (Listof (Pairof Symbol String))
                    (list (cons 'name (symbol->string name)))]
         [type-list : (Listof (Pairof Symbol String))
                    (list (cons 'type (qname->string (type))))]
         [required-list : (Listof (Pairof Symbol String))
                        (if required
                            (list (cons 'use "required"))
                            '())])
     (make-xml-element
      ((xs attribute))
      (append
       name-list
       type-list
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
    (xs:element 'value xs:string 1 1))

  (define x-element : XExpr
    '(xs:element ((name "value") (type "xs:string"))))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace" '() (list a-element)))
                (list 'xs:schema '((xmlns:xs        "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns       "urn:target-namespace")
                                   (targetNamespace "urn:target-namespace"))
                      x-element))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace"
                                      '((blub . "urn:bla"))
                                      (list a-element)))
                (list 'xs:schema '((xmlns:xs        "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns       "urn:target-namespace")
                                   (xmlns:blub      "urn:bla")
                                   (targetNamespace "urn:target-namespace"))
                      x-element))

  (check-equal? (xs->xexpr a-element)
                x-element)

  (check-equal? (xs->xexpr (xs:element 'blub (tns mytype) 2 3))
                '(xs:element ((name      "blub")
                              (type      "tns:mytype")
                              (minOccurs "2")
                              (maxOccurs "3"))))

  (let ([element (xs:element 'blub (tns mytype) 2 3)])
    (check-equal? (parameterize ([xs-prefix 'xsd]
                                 [tns-prefix 'tanaspa])
                    (xs->xexpr element))
                  '(xsd:element ((name      "blub")
                                 (type      "tanaspa:mytype")
                                 (minOccurs "2")
                                 (maxOccurs "3")))))


  (check-equal? (xs->xexpr (xs:simple-type 'atype xs:string '()))
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string")))))
  
  (check-equal? (xs->xexpr (xs:simple-type 'atype xs:string (list (xs:enumeration "bla"))))
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string"))
                                                (xs:enumeration ((value "bla"))))))

  (check-equal? (xs->xexpr (xs:min-inclusive 5))
                '(xs:minInclusive ((value "5"))))

  (check-equal? (xs->xexpr (xs:min-exclusive 5))
                '(xs:minExclusive ((value "5"))))

  (check-equal? (xs->xexpr (xs:max-inclusive 5))
                '(xs:maxInclusive ((value "5"))))

  (check-equal? (xs->xexpr (xs:max-exclusive 5))
                '(xs:maxExclusive ((value "5"))))

  (check-equal? (xs->xexpr (xs:enumeration "blub"))
                '(xs:enumeration ((value "blub"))))

  (check-equal? (xs->xexpr (xs:pattern "blub"))
                '(xs:pattern ((value "blub"))))

  (check-equal? (xs->xexpr (xs:length 5))
                '(xs:length ((value "5"))))

  (check-equal? (xs->xexpr (xs:min-length 5))
                '(xs:minLength ((value "5"))))

  (check-equal? (xs->xexpr (xs:max-length 5))
                '(xs:maxLength ((value "5"))))

  (check-equal? (xs->xexpr (xs:complex-type 'atype '()))
                '(xs:complex-type ((name "atype"))))

  (check-equal? (xs->xexpr
                 (xs:complex-type 'atype
                                  (list (xs:attribute 'prodid xs:string #f))))
                '(xs:complex-type ((name "atype"))
                                  (xs:attribute ((name "prodid")
                                                 (type "xs:string")))))

  (check-equal? (xs->xexpr (xs:attribute 'prodid xs:string #f))
                '(xs:attribute ((name "prodid")
                                (type "xs:string"))))

  (check-equal? (xs->xexpr (xs:attribute 'prodid xs:string #t))
                '(xs:attribute ((name    "prodid")
                                (type    "xs:string")
                                (use     "required"))))

  (check-equal? (xs->xexpr (xs:sequence 1 1 '()))
                '(xs:sequence ()))

  (check-equal? (xs->xexpr (xs:sequence 2 3 (list a-element)))
                (list 'xs:sequence '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (xs->xexpr (xs:all 1 1 '()))
                '(xs:all ()))

  (check-equal? (xs->xexpr (xs:all 2 3 (list a-element)))
                (list 'xs:all '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (xs->xexpr (xs:choice 1 1 '()))
                '(xs:choice ()))

  (check-equal? (xs->xexpr (xs:choice 2 3 (list a-element)))
                (list 'xs:choice '((minOccurs "2") (maxOccurs "3")) x-element))
)
     

