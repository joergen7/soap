#lang typed/racket/base

(require
  (only-in racket/match
           define/match
           match)
  (only-in typed/xml
           XExpr)
  (only-in racket/set
           set
           set-add
           list->set)
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
 get-provide-set
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
   [import-table     : (HashTable Symbol (U xs:import xs:schema))]
   [body             : (HashTable Symbol xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(define-predicate xs:schema-member?
  xs:schema-member)
       
(struct xs:import
  ([namespace   : String]
   [provide-set : (Setof Symbol)]))

(struct xs:element
  ([type       : (-> qname)]
   [min-occurs : Nonnegative-Integer]
   [max-occurs : (U #f Nonnegative-Integer)]))

;; simple-type

(struct xs:simple-type
  ([base : (-> qname)]
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
  ([attribute-table : (HashTable Symbol xs:attribute)]
   [body            : xs:complex-type-member]))

(define-type xs:complex-type-member
  (U xs:sequence
     xs:all
     xs:choice))

(define-predicate xs:complex-type-member?
  xs:complex-type-member)

(struct xs:attribute
  ([type     : (-> qname)]
   [required : Boolean]))

(struct xs:sequence
  ([body : (HashTable Symbol xs:element)]))

(struct xs:all
  ([body : (HashTable Symbol xs:element)]))

(struct xs:choice
  ([min-occurs : Nonnegative-Integer]
   [max-occurs : (U #f Nonnegative-Integer)]
   [body       : (HashTable Symbol xs:element)]))


(: get-provide-set (-> (U xs:import xs:schema) (Setof Symbol)))
(define/match (get-provide-set o)
  [((xs:schema _target-namespace _import-list body))
   (list->set (filter (λ (x) (not (xs:element? (hash-ref body x)))) (hash-keys body)))]
  [((xs:import _namespace provide-set))
   provide-set])

(: get-occur-list (-> Nonnegative-Integer (U #f Nonnegative-Integer) (Listof (Pairof Symbol String))))
(define (get-occur-list min-occurs max-occurs)
  (let ([l1 : (Listof (Pairof Symbol String))
            (list (cons 'minOccurs (number->string min-occurs)))]
        [l2 : (Listof (Pairof Symbol String))
            (list (cons 'maxOccurs (if max-occurs (number->string max-occurs) "unbounded")))])
    (append l1 l2)))

(: get-import-attribute-list (-> (HashTable Symbol (U xs:import xs:schema)) (Listof (Pairof Symbol String))))
(define (get-import-attribute-list import-table)

  (: proc (-> Symbol (U xs:import xs:schema) (Pairof Symbol String)))
  (define (proc prefix elem)
    (match elem
      [(xs:import namespace _provide-set)
       (cons prefix namespace)]
      [(xs:schema target-namespace _import-list _body)
       (cons prefix target-namespace)]))

  (hash-map import-table proc))

(: get-import-xexpr-list (-> (HashTable Symbol (U xs:import xs:schema)) qname (Listof XExpr)))
(define (get-import-xexpr-list import-table qn)

  (: proc (-> Symbol (U xs:import xs:schema) XExpr))
  (define (proc prefix elem)
    (match elem
      [(xs:import namespace _)
       (make-xml-element
        qn
        #:att-list (list (cons 'namespace namespace)))]
      [(xs:schema target-namespace _import-list _body)
       (make-xml-element
        qn
        #:att-list (list (cons 'namespace target-namespace)))]))

  (hash-map import-table proc))
  
  

(: xs->xexpr (->* (Any) (#:name-value (U #f Symbol)) XExpr))
(define (xs->xexpr x #:name-value (name-value #f))

  (: proc (-> Symbol Any XExpr))
  (define (proc name elem)
    (xs->xexpr elem #:name-value name))

  (match x
    
    ;; xs:schema
    [(xs:schema target-namespace import-table body)
     (let ([a-prefix-list : (Listof (Pairof Symbol String))
                          (get-import-attribute-list import-table)]
           [b-prefix-list : (Listof (Pairof Symbol String))
                          (list (cons (xs-prefix) "http://www.w3.org/2001/XMLSchema")
                                (cons (tns-prefix) target-namespace))]
           [att-list : (Listof (Pairof Symbol String))
                     (list (cons 'targetNamespace target-namespace)
                           (cons 'elementFormDefault "qualified"))]
           [import-list : (Listof XExpr)
                        (get-import-xexpr-list import-table ((xs import)))]
           [body-list : (Listof XExpr)
                      (hash-map body proc)])
       (make-xml-element
        ((xs schema))
        #:prefix-list (append b-prefix-list a-prefix-list)
        #:att-list    att-list
        #:body        (append import-list body-list)))]

  ;; xs:element
    [(xs:element type min-occurs max-occurs)
     (let ([occur-list : (Listof (Pairof Symbol String))
                       (get-occur-list min-occurs max-occurs)]
           [type-list : (Listof (Pairof Symbol String))
                      (list (cons 'type (qname->string (type))))])
       (make-xml-element
        ((xs element))
        #:att-list   (append
                      type-list
                      occur-list)
        #:name-value name-value))]

    ;; xs:simple-type
    [(xs:simple-type base body)
     (make-xml-element
      ((xs simpleType))
      #:name-value name-value
      #:body (list
              (make-xml-element
               ((xs restriction))
               #:att-list   (list (cons 'base (qname->string (base))))
               #:body       (map xs->xexpr body))))]
    
    ;; xs:min-inclusive
    [(xs:min-inclusive value)
     (make-xml-element
      ((xs minInclusive))
      #:att-list (list (cons 'value (number->string value))))]
    
    ;; xs:min-exclusive
    [(xs:min-exclusive value)
     (make-xml-element
      ((xs minExclusive))
      #:att-list (list (cons 'value (number->string value))))]
    
    ;; xs:max-inclusive
    [(xs:max-inclusive value)
     (make-xml-element
      ((xs maxInclusive))
      #:att-list (list (cons 'value (number->string value))))]

    ;; xs:max-exclusive
    [(xs:max-exclusive value)
     (make-xml-element
      ((xs maxExclusive))
      #:att-list (list (cons 'value (number->string value))))]
    
    ;; xs:enumeration
    [(xs:enumeration value)
     (make-xml-element
      ((xs enumeration))
      #:att-list (list (cons 'value value)))]

    ;; xs:pattern
    [(xs:pattern value)
     (make-xml-element
      ((xs pattern))
      #:att-list (list (cons 'value value)))]

    ;; xs:length
    [(xs:length value)
     (make-xml-element
      ((xs length))
      #:att-list (list (cons 'value (number->string value))))]

    ;; xs:min-length
    [(xs:min-length value)
     (make-xml-element
      ((xs minLength))
      #:att-list (list (cons 'value (number->string value))))]

    ;; xs:max-length
    [(xs:max-length value)
     (make-xml-element
      ((xs maxLength))
      #:att-list (list (cons 'value (number->string value))))]

    ;; xs:complex-type
    [(xs:complex-type attribute-table body)
     (make-xml-element
      ((xs complex-type))
      #:name-value name-value
      #:body (append
              (hash-map attribute-table proc)
              (list (xs->xexpr body))))]

    ;; xs:attribute
    [(xs:attribute type required)
     (let ([type-list : (Listof (Pairof Symbol String))
                      (list (cons 'type (qname->string (type))))]
           [required-list : (Listof (Pairof Symbol String))
                          (if required
                              (list (cons 'use "required"))
                              '())])
       (make-xml-element
        ((xs attribute))
        #:att-list   (append
                      type-list
                      required-list)
        #:name-value name-value))]

    ;; xs:sequence
    [(xs:sequence body)
     (make-xml-element
      ((xs sequence))
      #:body (hash-map body proc))]
    
    ;; xs:all
    [(xs:all body)
     (make-xml-element
      ((xs all))
      #:body (hash-map body proc))]
    
    ;; xs:choice
    [(xs:choice min-occurs max-occurs body)
     (make-xml-element
      ((xs choice))
      #:att-list (get-occur-list min-occurs max-occurs)
      #:body     (hash-map body proc))]))

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
    (xs:element xs:string 1 1))

  (define x-element : XExpr
    '(xs:element ((name "value") (type "xs:string") (minOccurs "1") (maxOccurs "1"))))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace" (hash) (hash 'value a-element)))
                (list 'xs:schema '((xmlns:xs           "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns          "urn:target-namespace")
                                   (targetNamespace    "urn:target-namespace")
                                   (elementFormDefault "qualified"))
                      x-element))

  (check-equal? (xs->xexpr (xs:schema "urn:target-namespace"
                                      (hash 'blub (xs:import "urn:bla" (set)))
                                      (hash 'value a-element)))
                (list 'xs:schema '((xmlns:xs           "http://www.w3.org/2001/XMLSchema")
                                   (xmlns:tns          "urn:target-namespace")
                                   (xmlns:blub         "urn:bla")
                                   (targetNamespace    "urn:target-namespace")
                                   (elementFormDefault "qualified"))
                      '(xs:import ((namespace "urn:bla")))
                      x-element))

  (check-equal? (parameterize ([xs-prefix 'xsd]
                               [tns-prefix 't])
                  (xs->xexpr (xs:schema "urn:target-namespace" (hash) (hash 'value a-element))))
                  (list 'xsd:schema '((xmlns:xsd          "http://www.w3.org/2001/XMLSchema")
                                      (xmlns:t            "urn:target-namespace")
                                      (targetNamespace    "urn:target-namespace")
                                      (elementFormDefault "qualified"))
                        '(xsd:element ((name "value") (type "xsd:string") (minOccurs "1") (maxOccurs "1")))))

  (check-equal? (xs->xexpr a-element #:name-value 'value)
                x-element)

  (check-equal? (xs->xexpr (xs:element (tns mytype) 2 3) #:name-value 'blub)
                '(xs:element ((name      "blub")
                              (type      "tns:mytype")
                              (minOccurs "2")
                              (maxOccurs "3"))))

  (let ([element (xs:element (tns mytype) 2 3)])
    (check-equal? (parameterize ([xs-prefix 'xsd]
                                 [tns-prefix 'tanaspa])
                    (xs->xexpr element #:name-value 'blub))
                  '(xsd:element ((name      "blub")
                                 (type      "tanaspa:mytype")
                                 (minOccurs "2")
                                 (maxOccurs "3")))))


  (check-equal? (xs->xexpr (xs:simple-type xs:string '()) #:name-value 'atype)
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string")))))
  
  (check-equal? (xs->xexpr (xs:simple-type xs:string (list (xs:enumeration "bla"))) #:name-value 'atype)
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

  (check-equal? (xs->xexpr (xs:complex-type (hash) (xs:all (hash))) #:name-value 'atype)
                '(xs:complex-type ((name "atype")) (xs:all ())))

  (check-equal? (xs->xexpr
                 (xs:complex-type (hash 'prodid (xs:attribute xs:string #f)) (xs:all (hash))) #:name-value 'atype)
                '(xs:complex-type ((name "atype"))
                                  (xs:attribute ((name "prodid")
                                                 (type "xs:string")))
                                  (xs:all ())))

  (check-equal? (xs->xexpr (xs:attribute xs:string #f) #:name-value 'prodid)
                '(xs:attribute ((name "prodid")
                                (type "xs:string"))))

  (check-equal? (xs->xexpr (xs:attribute xs:string #t) #:name-value 'prodid)
                '(xs:attribute ((name    "prodid")
                                (type    "xs:string")
                                (use     "required"))))

  (check-equal? (xs->xexpr (xs:sequence (hash)))
                '(xs:sequence ()))

  (check-equal? (xs->xexpr (xs:sequence (hash 'value a-element)))
                (list 'xs:sequence '() x-element))

  (check-equal? (xs->xexpr (xs:all (hash)))
                '(xs:all ()))

  (check-equal? (xs->xexpr (xs:all (hash 'value a-element)))
                (list 'xs:all '() x-element))

  (check-equal? (xs->xexpr (xs:choice 1 1 (hash)))
                '(xs:choice ((minOccurs "1") (maxOccurs "1"))))

  (check-equal? (xs->xexpr (xs:choice 2 3 (hash 'value a-element)))
                (list 'xs:choice '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" (hash) (hash)))
                (set))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" (hash)
                                                  (hash 'bla (xs:element xs:string 1 1))))
                (set))

  (check-equal? (get-provide-set (xs:schema "urn:target-namespace" (hash)
                                                  (hash 'bla (xs:element xs:string 1 1)
                                                        'blub (xs:simple-type xs:string '())
                                                        'foo (xs:complex-type (hash) (xs:all (hash))))))
                (set 'foo 'blub))

  )
     

