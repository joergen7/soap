#lang typed/racket/base

(require
  (only-in racket/match
           define/match)
  (only-in typed/xml
           XExpr)
  (only-in racket/match
           match)
  (only-in racket/set
           set
           set-add)
  "xml-aux.rkt")

(provide
 xs:string
 xs:decimal
 xs:integer
 xs:unsigned-short
 xs:non-negative-integer
 xs:boolean
 xs:date
 xs:time
 xs:date-time
 (struct-out xs:schema)
 xs:schema-member
 xs:schema-member?
 (struct-out xs:import)
 get-provide-set
 (struct-out xs:element)
 (struct-out xs:simple-type)
 xs:simple-type-member
 xs:simple-type-member?
 (struct-out xs:min-inclusive)
 (struct-out xs:min-exclusive)
 (struct-out xs:max-inclusive)
 (struct-out xs:max-exclusive)
 (struct-out xs:enumeration)
 (struct-out xs:pattern)
 (struct-out xs:length)
 (struct-out xs:min-length)
 (struct-out xs:max-length)
 (struct-out xs:complex-type)
 xs:complex-type-member
 xs:complex-type-member?
 (struct-out xs:attribute)
 (struct-out xs:sequence)
 (struct-out xs:all)
 (struct-out xs:choice)
 get-import-attribute-list
 get-import-xexpr-list
 xs->xexpr
 xs-validate-schema)

;; base types

(define xs:string
  (xs string))

(define xs:decimal
  (xs decimal))

(define xs:integer
  (xs integer))

(define xs:unsigned-short
  (xs unsignedShort))

(define xs:non-negative-integer
  (xs nonNegativeInteger))

(define xs:boolean
  (xs boolean))

(define xs:date
  (xs date))

(define xs:time
  (xs time))

(define xs:date-time
  (xs dateTime))

;; schema

(struct xs:schema
  ([target-namespace : String]
   [import-list      : (Listof (U xs:import (Pairof Symbol xs:schema)))]
   [body             : (Listof xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(define-predicate xs:schema-member?
  xs:schema-member)

(: get-provide-set (-> (U xs:import xs:schema) (Setof Symbol)))
(define/match (get-provide-set o)
  [((xs:schema _target-namespace _import-list body))
   (for/fold ([result : (Setof Symbol) (set)])
             ([x : xs:schema-member (in-list body)])
     (match x
       [(xs:element _name _type _min-occurs _max-occurs) result]
       [(xs:simple-type name _base _body)                (set-add result name)]
       [(xs:complex-type name _body)                     (set-add result name)]))]
  [((xs:import _prefix _namespace provide-set))
   provide-set])
       
(struct xs:import
  ([prefix      : Symbol]
   [namespace   : String]
   [provide-set : (Setof Symbol)]))

(struct xs:element
  ([name       : Symbol]
   [type       : (-> qname)]
   [min-occurs : Nonnegative-Integer]
   [max-occurs : (U #f Nonnegative-Integer)]))

;; simple-type

(struct xs:simple-type
  ([name : Symbol]
   [base : (-> qname)]
   [body : (Listof xs:simple-type-member)]))

(define-type xs:simple-type-member
  (U xs:min-inclusive
     xs:min-exclusive
     xs:max-inclusive
     xs:max-exclusive
     xs:enumeration
     xs:pattern
     xs:length
     xs:min-length
     xs:max-length))

(define-predicate xs:simple-type-member?
  xs:simple-type-member)

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

(define-predicate xs:complex-type-member?
  xs:complex-type-member)

(struct xs:attribute
  ([name     : Symbol]
   [type     : (-> qname)]
   [required : Boolean]))

(struct xs:sequence
  ([body       : (Listof xs:element)]))

(struct xs:all
  ([body       : (Listof xs:element)]))

(struct xs:choice
  ([min-occurs : Nonnegative-Integer]
   [max-occurs : (U #f Nonnegative-Integer)]
   [body : (Listof xs:element)]))


(: get-occur-list (-> Nonnegative-Integer (U #f Nonnegative-Integer) (Listof (Pairof Symbol String))))
(define (get-occur-list min-occurs max-occurs)
  (let ([l1 : (Listof (Pairof Symbol String))
            (list (cons 'minOccurs (number->string min-occurs)))]
        [l2 : (Listof (Pairof Symbol String))
            (list (cons 'maxOccurs (if max-occurs (number->string max-occurs) "unbounded")))])
    (append l1 l2)))

(: get-import-attribute-list (-> (Listof (U xs:import (Pairof Symbol xs:schema))) (Listof (Pairof Symbol String))))
(define (get-import-attribute-list import-list)
  (for/list ([imp : (U xs:import (Pairof Symbol xs:schema)) (in-list import-list)])
    (match imp
      [(xs:import prefix namespace _)
       (cons prefix namespace)]
      [(cons prefix (xs:schema target-namespace _import-list _body))
       (cons prefix target-namespace)])))

(: get-import-xexpr-list (-> (Listof (U xs:import (Pairof Symbol xs:schema))) qname (Listof XExpr)))
(define (get-import-xexpr-list import-list qn)
  (for/list ([imp : (U xs:import (Pairof Symbol xs:schema)) (in-list import-list)])
    (match imp
      [(xs:import _prefix namespace _)
       (make-xml-element
        qn
        (list (cons 'namespace namespace)))]
      [(cons prefix (xs:schema target-namespace _import-list _body))
       (make-xml-element
        qn
        (list (cons 'namespace target-namespace)))])))
  

(: xs->xexpr (-> Any XExpr))
(define/match (xs->xexpr x)

  ;; xs:schema
  [((xs:schema target-namespace import-list body))
   (let ([a-prefix-list : (Listof (Pairof Symbol String))
                        (get-import-attribute-list import-list)]
         [b-prefix-list : (Listof (Pairof Symbol String))
                        (list (cons (xs-prefix) "http://www.w3.org/2001/XMLSchema")
                              (cons (tns-prefix) target-namespace))]
         [att-list : (Listof (Pairof Symbol String))
                   (list (cons 'targetNamespace target-namespace)
                         (cons 'elementFormDefault "qualified"))]
         [import-list : (Listof XExpr)
                      (get-import-xexpr-list import-list ((xs import)))]
         [body-list : (Listof XExpr)
                    (map xs->xexpr body)])
     (make-xml-element
      ((xs schema))
      #:prefix-list (append b-prefix-list a-prefix-list)
      att-list
      (append import-list body-list)))]

  ;; xs:element
  [((xs:element name type min-occurs max-occurs))
   (let ([occur-list : (Listof (Pairof Symbol String))
                     (get-occur-list min-occurs max-occurs)]
         [name-list : (Listof (Pairof Symbol String))
                    (list (cons 'name (symbol->string name)))]
         [type-list : (Listof (Pairof Symbol String))
                    (list (cons 'type (qname->string (type))))])
   (make-xml-element
    ((xs element))
    (append
     name-list
     type-list
     occur-list)))]

  ;; xs:simple-type
  [((xs:simple-type name base body))
   (make-xml-element
    ((xs simpleType))
    (list (cons 'name (symbol->string name)))
     (list
      (make-xml-element
       ((xs restriction))
       (list (cons 'base (qname->string (base))))
       (map xs->xexpr body))))]

  ;; xs:min-inclusive
  [((xs:min-inclusive value))
   (make-xml-element
    ((xs minInclusive))
    (list (cons 'value (number->string value))))]
  
  ;; xs:min-exclusive
  [((xs:min-exclusive value))
   (make-xml-element
    ((xs minExclusive))
    (list (cons 'value (number->string value))))]

  ;; xs:max-inclusive
  [((xs:max-inclusive value))
   (make-xml-element
    ((xs maxInclusive))
    (list (cons 'value (number->string value))))]

  ;; xs:max-exclusive
  [((xs:max-exclusive value))
   (make-xml-element
    ((xs maxExclusive))
    (list (cons 'value (number->string value))))]

  ;; xs:enumeration
  [((xs:enumeration value))
   (make-xml-element
    ((xs enumeration))
    (list (cons 'value value)))]

  ;; xs:pattern
  [((xs:pattern value))
   (make-xml-element
    ((xs pattern))
    (list (cons 'value value)))]

  ;; xs:length
  [((xs:length value))
   (make-xml-element
    ((xs length))
    (list (cons 'value (number->string value))))]

  ;; xs:min-length
  [((xs:min-length value))
   (make-xml-element
    ((xs minLength))
    (list (cons 'value (number->string value))))]

  ;; xs:max-length
  [((xs:max-length value))
   (make-xml-element
    ((xs maxLength))
    (list (cons 'value (number->string value))))]

  ;; xs:complex-type
  [((xs:complex-type name body))
   (make-xml-element
    ((xs complex-type))
    (list (cons 'name (symbol->string name)))
    (map xs->xexpr body))]

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
       required-list)))]

  ;; xs:sequence
  [((xs:sequence body))
   (make-xml-element
    ((xs sequence))
    '()
     (map xs->xexpr body))]

  ;; xs:all
  [((xs:all body))
   (make-xml-element
    ((xs all))
    '()
    (map xs->xexpr body))]

;; xs:choice
  [((xs:choice min-occurs max-occurs body))
   (make-xml-element
    ((xs choice))
    (get-occur-list min-occurs max-occurs)
    (map xs->xexpr body))])

(: xs-validate-schema (-> xs:schema xs:schema))
(define (xs-validate-schema schema)
  (xs-validate schema)
  schema)

(: xs-validate (-> Any Any))
(define/match (xs-validate x)
  ;; TODO:
  ;; for any qname, make sure the prefix is bound to a namespace
  ;; enforce minOccurrence <= maxOccurrence
  ;; for any type reference make sure it is defined locally
  )

(module+ test

  (require typed/rackunit)
  
  (define a-element : xs:element
    (xs:element 'value xs:string 1 1))

  (define x-element : XExpr
    '(xs:element ((name "value") (type "xs:string") (minOccurs "1") (maxOccurs "1"))))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace" '() (list a-element)))
                (list 'xs:schema '((xmlns:xs           "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns          "urn:target-namespace")
                                   (targetNamespace    "urn:target-namespace")
                                   (elementFormDefault "qualified"))
                      x-element))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace"
                                      (list (xs:import 'blub "urn:bla" (set)))
                                      (list a-element)))
                (list 'xs:schema '((xmlns:xs           "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns          "urn:target-namespace")
                                   (xmlns:blub         "urn:bla")
                                   (targetNamespace    "urn:target-namespace")
                                   (elementFormDefault "qualified"))
                      '(xs:import ((namespace "urn:bla")))
                      x-element))

  (check-equal? (parameterize ([xs-prefix 'xsd]
                               [tns-prefix 't])
                  (xs->xexpr (xs:schema "urn:target-namespace" '() (list a-element))))
                  (list 'xsd:schema '((xmlns:xsd          "http://www.w3.org/2001/XMLSchema")
                                      (xmlns:t            "urn:target-namespace")
                                      (targetNamespace    "urn:target-namespace")
                                      (elementFormDefault "qualified"))
                        '(xsd:element ((name "value") (type "xsd:string") (minOccurs "1") (maxOccurs "1")))))

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

  (check-equal? (xs->xexpr (xs:sequence '()))
                '(xs:sequence ()))

  (check-equal? (xs->xexpr (xs:sequence (list a-element)))
                (list 'xs:sequence '() x-element))

  (check-equal? (xs->xexpr (xs:all '()))
                '(xs:all ()))

  (check-equal? (xs->xexpr (xs:all (list a-element)))
                (list 'xs:all '() x-element))

  (check-equal? (xs->xexpr (xs:choice 1 1 '()))
                '(xs:choice ((minOccurs "1") (maxOccurs "1"))))

  (check-equal? (xs->xexpr (xs:choice 2 3 (list a-element)))
                (list 'xs:choice '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" '() '()))
                (set))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" '()
                                                  (list
                                                   (xs:element 'bla xs:string 1 1))))
                (set))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" '()
                                                  (list
                                                   (xs:element 'bla xs:string 1 1)
                                                   (xs:simple-type 'blub xs:string '())
                                                   (xs:complex-type 'foo '()))))
                (set 'foo 'blub))

  )
     

