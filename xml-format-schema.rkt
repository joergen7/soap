#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          set
          in-set)
 (only-in typed/xml
          XExpr)
 "xml-schema.rkt"
 "xml-format.rkt"
 "xml-element.rkt")

;; xs:get-occur-list
;;------------------------------------------------------------

(: xs:get-occur-list (-> Nonnegative-Integer
                      (U #f Nonnegative-Integer)
                      (Listof (Pairof Symbol String))))
(define (xs:get-occur-list min-occurs max-occurs)
  (let ([l1 : (Listof (Pairof Symbol String))
            (if (= min-occurs 1)
                '()
                (list (cons 'minOccurs (number->string min-occurs))))]
        [l2 : (Listof (Pairof Symbol String))
            (if (and (= min-occurs 1)
                     max-occurs
                     (= max-occurs 1))
                '()
                (list (cons 'maxOccurs (if max-occurs (number->string max-occurs) "unbounded"))))])
    (append l1 l2)))

(provide xs:get-occur-list)


;; xs:formattable
;;------------------------------------------------------------

(define-type xs:formattable
  (U xs:schema
     xs:element
     xs:simple-type
     xs:complex-type
     xs:min-inclusive
     xs:min-exclusive
     xs:max-inclusive
     xs:max-exclusive
     xs:enumeration
     xs:pattern
     xs:length
     xs:min-length
     xs:max-length
     xs:attribute
     xs:all
     xs:choice
     xs:restriction))

(define-predicate xs:formattable?
  xs:formattable)
   
(provide xs:formattable
         xs:formattable?)


;; xs:xs->xexpr
;;------------------------------------------------------------

(: xs:xs->xexpr (-> xs:formattable XExpr))
(define/match (xs:xs->xexpr o)

  ;; xs:schema
  [((xs:schema name namespace body))

   (define member-table : (Listof (Pairof Symbol xs:schema-member))
     (xml:alist-apply-union
      xml:collect-member-table
      body))

   (define xml-body : (Listof XExpr)
     (for/list ([p : (Pairof Symbol xs:schema-member)
                   (in-list member-table)])
       (match p
         [(cons _name m)
          (xs:xs->xexpr m)])))

   (define import-table : (Listof (Pairof Symbol String))
     (xs:collect-import-table o))

   (define import-body : (Listof XExpr)
     (for/fold ([result : (Listof XExpr)
                        '()])
               ([p : (Pairof Symbol String)
                   (in-list import-table)])
       (match p
         [(cons prf ns)
          (if (hash-has-key? xml:prefix-table prf)
              result
              (cons
               (make-xml-element
                'xs:import
                #:att-list (list
                            (cons 'namespace ns)))
               result))])))

   (make-xml-element
    'xs:schema
    #:name-value  name
    #:prefix-list (xs:extend-import-table
                   import-table
                   namespace
                   xml:prefix-xs)
    #:att-list    (list
                   (cons 'targetNamespace      namespace)
                   (cons 'elementFormDefault   "qualified")
                   (cons 'attributeFormDefault "unqualified"))
    #:body        (append
                   import-body
                   xml-body))]

  ;; xs:element
  [((xs:element name type min-occurs max-occurs))
   
   (define type-string : String
     (xs:type->string type))

   (make-xml-element
    'xs:element
    #:name-value name
    #:att-list   (append
                  (list
                   (cons 'type type-string))
                  (xs:get-occur-list
                   min-occurs
                   max-occurs)))]


  ;; xs:simple-type
  [((xs:simple-type name restriction))
   (make-xml-element
    'xs:simpleType
    #:name-value name
    #:body       (list
                  (xs:xs->xexpr restriction)))]

  ;; xs:restriction
  [((xs:restriction base body))

   (define xml-body : (Listof XExpr)
     (for/list ([x : xs:restriction-member
                   (in-set body)])
       (xs:xs->xexpr x)))

   (make-xml-element
    'xs:restriction
    #:att-list (list
                (cons 'base (xs:type->string base)))
    #:body     xml-body)]

  ;; xs:min-inclusive
  [((xs:min-inclusive value))

   (make-xml-element
    'xs:minInclusive
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:min-exclusive
  [((xs:min-exclusive value))

   (make-xml-element
    'xs:minExclusive
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:max-inclusive
  [((xs:max-inclusive value))

   (make-xml-element
    'xs:maxInclusive
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:max-exclusive
  [((xs:max-exclusive value))

   (make-xml-element
    'xs:maxExclusive
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:enumeration
  [((xs:enumeration value))

   (make-xml-element
    'xs:enumeration
    #:att-list (list
                (cons 'value value)))]

  ;; xs:pattern
  [((xs:pattern value))

   (make-xml-element
    'xs:pattern
    #:att-list (list
                (cons 'value value)))]

  ;; xs:length
  [((xs:length value))

   (make-xml-element
    'xs:length
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:min-length
  [((xs:min-length value))

   (make-xml-element
    'xs:minLength
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:max-length
  [((xs:max-length value))

   (make-xml-element
    'xs:maxLength
    #:att-list (list
                (cons 'value (number->string value))))]

  ;; xs:complex-type
  [((xs:complex-type name attribute-set body))

   (define l1 : (Listof XExpr)
     (for/list ([a : xs:attribute
                   (in-set attribute-set)])
       (xs:xs->xexpr a)))

   (define l2 : (Listof XExpr)
     (if body
         (list (xs:xs->xexpr body))
         '()))

   (make-xml-element
    'xs:complexType
    #:name-value name
    #:body       (append l1 l2))]

  ;; xs:all
  [((xs:all element-set))

   (define xml-body : (Listof XExpr)
     (for/list ([x : xs:element
                   (in-set element-set)])
       (xs:xs->xexpr x)))

   (make-xml-element
    'xs:all
    #:body xml-body)]

  ;; xs:choice
  [((xs:choice min-occurs max-occurs element-set))

   (define occur-list : (Listof (Pairof Symbol String))
     (xs:get-occur-list min-occurs max-occurs))

   (define xml-body : (Listof XExpr)
     (for/list ([x : xs:element
                   (in-set element-set)])
       (xs:xs->xexpr x)))

   (make-xml-element
    'xs:choice
    #:att-list occur-list
    #:body     xml-body)]
  
  ;; xs:attribute
  [((xs:attribute name type required))

   (define l1 : (Listof (Pairof Symbol String))
     (list
      (cons 'type (xs:type->string type))))

   (define l2 : (Listof (Pairof Symbol String))
     (if required
         (list
          (cons 'use "required"))
         '()))
   
   (make-xml-element
    'xs:attribute
    #:name-value name
    #:att-list   (append l1 l2))])

(provide xs:xs->xexpr)





(module+ test

  (require typed/rackunit)

  (define a-element : xs:element
    (xs:element
     'value
     (xs string)
     1
     1))

  (define x-element : XExpr
    '(xs:element
      ((name "value")
       (type "xs:string"))))

  (define a-restriction : xs:restriction
    (xs:restriction
     (xs string)
     (set)))

  (check-equal? (xs:get-occur-list 0 0)
                '((minOccurs . "0")
                  (maxOccurs . "0")))

  (check-equal? (xs:get-occur-list 0 1)
                '((minOccurs . "0")
                  (maxOccurs . "1")))

  (check-equal? (xs:get-occur-list 1 1)
                '())

  (check-equal? (xs:get-occur-list 1 2)
                '((maxOccurs . "2")))

  (check-equal? (xs:get-occur-list 2 2)
                '((minOccurs . "2")
                  (maxOccurs . "2")))

  (check-equal? (xs:get-occur-list 1 #f)
                '((maxOccurs . "unbounded")))

  (check-equal? (xs:type->string
                 (xs string))
                "xs:string")

  (check-equal? (xs:type->string
                 (xs:simple-type
                  'blub
                  a-restriction))
                "tns:blub")

  (check-equal? (xs:type->string
                 (xs:complex-type
                  'bla
                  (set)
                  #f))
                "tns:bla")
                  
  
  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:target-namespace"
                  (set)))
                (list
                 'xs:schema
                 '((name                 "schema")
                   (xmlns:tns            "urn:target-namespace")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (targetNamespace      "urn:target-namespace")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))))

  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:target-namespace"
                  (set a-element)))
                (list
                 'xs:schema
                 '((name                 "schema")
                   (xmlns:tns            "urn:target-namespace")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (targetNamespace      "urn:target-namespace")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))
                 x-element))

  (check-equal? (xs:xs->xexpr a-element)
                x-element)

  (check-equal? (xs:xs->xexpr (xs:element 'blub (xs string) 2 3))
                '(xs:element ((name      "blub")
                              (type      "xs:string")
                              (minOccurs "2")
                              (maxOccurs "3"))))

  (check-equal? (xs:xs->xexpr (xs:simple-type 'atype a-restriction))
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string")))))
  
  (check-equal? (xs:xs->xexpr (xs:simple-type 'atype (xs:restriction (xs string) (set (xs:enumeration "bla")))))
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string"))
                                                (xs:enumeration ((value "bla"))))))

  (check-equal? (xs:xs->xexpr (xs:min-inclusive 5))
                '(xs:minInclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:min-exclusive 5))
                '(xs:minExclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-inclusive 5))
                '(xs:maxInclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-exclusive 5))
                '(xs:maxExclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:enumeration "blub"))
                '(xs:enumeration ((value "blub"))))

  (check-equal? (xs:xs->xexpr (xs:pattern "blub"))
                '(xs:pattern ((value "blub"))))

  (check-equal? (xs:xs->xexpr (xs:length 5))
                '(xs:length ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:min-length 5))
                '(xs:minLength ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-length 5))
                '(xs:maxLength ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:complex-type 'atype (set) #f))
                '(xs:complexType ((name "atype"))))

  (check-equal? (xs:xs->xexpr (xs:complex-type 'atype (set) (xs:all (set))))
                '(xs:complexType ((name "atype")) (xs:all ())))

  (check-equal? (xs:xs->xexpr
                 (xs:complex-type 'atype (set (xs:attribute 'prodid (xs string) #f)) (xs:all (set))))
                '(xs:complexType ((name "atype"))
                                 (xs:attribute ((name "prodid")
                                                (type "xs:string")))
                                 (xs:all ())))

  (check-equal? (xs:xs->xexpr (xs:attribute 'prodid (xs string) #f))
                '(xs:attribute ((name "prodid")
                                (type "xs:string"))))

  (check-equal? (xs:xs->xexpr (xs:attribute 'prodid (xs string) #t))
                '(xs:attribute ((name    "prodid")
                                (type    "xs:string")
                                (use     "required"))))

  (check-equal? (xs:xs->xexpr (xs:all (set)))
                '(xs:all ()))

  (check-equal? (xs:xs->xexpr (xs:all (set a-element)))
                (list 'xs:all '() x-element))

  (check-equal? (xs:xs->xexpr (xs:choice 1 1 (set)))
                '(xs:choice ()))

  (check-equal? (xs:xs->xexpr (xs:choice 2 3 (set a-element)))
                (list 'xs:choice '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:schema"
                  (set
                   (xs:simple-type
                    'atype
                    (xs:restriction
                     (xs:qname
                      (xs:import 'z "urn:z" (set 'blub) (set))
                      'blub)
                     (set))))))
                '(xs:schema
                  ((name                 "schema")
                   (xmlns:tns            "urn:schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:z              "urn:z")
                   (targetNamespace      "urn:schema")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))
                  (xs:import ((namespace "urn:z")))
                  (xs:simpleType
                   ((name "atype"))
                   (xs:restriction ((base "z:blub"))))))
                         
  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:schema"
                  (set
                   (xs:simple-type
                    'atype
                    (xs:restriction
                     (xs:simple-type
                      'btype
                      (xs:restriction
                       (xs:qname
                      (xs:import 'z "urn:z" (set 'blub) (set))
                      'blub)
                       (set)))
                     (set))))))
                '(xs:schema
                  ((name                 "schema")
                   (xmlns:tns            "urn:schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:z              "urn:z")
                   (targetNamespace      "urn:schema")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))
                  (xs:import ((namespace "urn:z")))
                  (xs:simpleType
                   ((name "atype"))
                   (xs:restriction ((base "tns:btype"))))
                  (xs:simpleType
                   ((name "btype"))
                   (xs:restriction ((base "z:blub")))))))
                         


