#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          in-set
          set
          set->list)
 (only-in typed/xml
          XExpr)
 "alist.rkt"
 "xs.rkt"
 "ns.rkt"
 "ns-form.rkt"
 "tns.rkt"
 "xml-element.rkt")


;; xs:get-occur-list
;;------------------------------------------------------------

(: xs:get-occur-list (-> Nonnegative-Integer
                      (U #f Nonnegative-Integer)
                      (Alistof String)))
(define (xs:get-occur-list min-occurs max-occurs)
  (let ([l1 : (Alistof String)
            (if (= min-occurs 1)
                '()
                (list (cons 'minOccurs (number->string min-occurs))))]
        [l2 : (Alistof String)
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

(: xs:xs->xexpr (-> xs:formattable
                    (-> Symbol String)
                    XExpr))
(define (xs:xs->xexpr o f)

  (match o

    ;; xs:schema
    [(xs:schema name namespace import-set body)

     (define xml-body : (Listof XExpr)
       (for/list ([m : xs:schema-member
                     (in-list
                      (sort (set->list body)
                            xs:schema-member<?))])
         (xs:xs->xexpr m f)))

     (define import-body : (Listof XExpr)
       (for/list ([x : xs:import
                     (in-list
                      (sort (set->list import-set)
                            xs:import<?))])
         (match x
           [(xs:import prefix ns _simple-provide-set _complex-provide-set)
            (make-xml-element
             (xs import)
             #:att-list (list (cons 'namespace ns)))])))

     (make-xml-element
      (xs schema)
      #:name-value  name
      #:prefix-list (xml:extend-import-table
                     import-set
                     namespace
                     xs:prefix)
      #:att-list    (list
                     (cons 'targetNamespace      namespace)
                     (cons 'elementFormDefault   "qualified")
                     (cons 'attributeFormDefault "unqualified"))
      #:body        (append
                     import-body
                     xml-body))]

    ;; xs:element
    [(xs:element name type min-occurs max-occurs)
     
     (define type-string : String
       (f type))

     (make-xml-element
      (xs element)
      #:name-value name
      #:att-list   (append
                    (list
                     (cons 'type type-string))
                    (xs:get-occur-list
                     min-occurs
                     max-occurs)))]


    ;; xs:simple-type
    [(xs:simple-type name restriction)
     (make-xml-element
      (xs simpleType)
      #:name-value name
      #:body       (list
                    (xs:xs->xexpr restriction f)))]

    ;; xs:restriction
    [(xs:restriction base body)

     (define xml-body : (Listof XExpr)
       (for/list ([x : xs:restriction-member
                     (in-set body)])
         (xs:xs->xexpr x f)))

     (make-xml-element
      (xs restriction)
      #:att-list (list
                  (cons 'base (f base)))
      #:body     xml-body)]

    ;; xs:min-inclusive
    [(xs:min-inclusive value)

     (make-xml-element
      (xs minInclusive)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:min-exclusive
    [(xs:min-exclusive value)

     (make-xml-element
      (xs minExclusive)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:max-inclusive
    [(xs:max-inclusive value)

     (make-xml-element
      (xs maxInclusive)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:max-exclusive
    [(xs:max-exclusive value)

     (make-xml-element
      (xs maxExclusive)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:enumeration
    [(xs:enumeration value)

     (make-xml-element
      (xs enumeration)
      #:att-list (list
                  (cons 'value value)))]

    ;; xs:pattern
    [(xs:pattern value)

     (make-xml-element
      (xs pattern)
      #:att-list (list
                  (cons 'value value)))]

    ;; xs:length
    [(xs:length value)

     (make-xml-element
      (xs length)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:min-length
    [(xs:min-length value)

     (make-xml-element
      (xs minLength)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:max-length
    [(xs:max-length value)

     (make-xml-element
      (xs maxLength)
      #:att-list (list
                  (cons 'value (number->string value))))]

    ;; xs:complex-type
    [(xs:complex-type name attribute-set body)

     (define l1 : (Listof XExpr)
       (for/list ([a : xs:attribute
                     (in-list
                      (sort (set->list attribute-set)
                            xs:attribute<?))])
         (xs:xs->xexpr a f)))

     (define l2 : (Listof XExpr)
       (if body
           (list (xs:xs->xexpr body f))
           '()))

     (make-xml-element
      (xs complexType)
      #:name-value name
      #:body       (append l1 l2))]

    ;; xs:all
    [(xs:all element-set)

     (define xml-body : (Listof XExpr)
       (for/list ([x : xs:element
                     (in-list
                      (sort (set->list element-set)
                            xs:element<?))])
         (xs:xs->xexpr x f)))

     (make-xml-element
      (xs all)
      #:body xml-body)]

    ;; xs:choice
    [(xs:choice min-occurs max-occurs element-set)

     (define occur-list : (Alistof String)
       (xs:get-occur-list min-occurs max-occurs))

     (define xml-body : (Listof XExpr)
       (for/list ([x : xs:element
                     (in-list
                      (sort (set->list element-set)
                            xs:element<?))])
         (xs:xs->xexpr x f)))

     (make-xml-element
      (xs choice)
      #:att-list occur-list
      #:body     xml-body)]
    
    ;; xs:attribute
    [(xs:attribute name type required)

     (define l1 : (Alistof String)
       (list
        (cons 'type (f type))))

     (define l2 : (Alistof String)
       (if required
           (list
            (cons 'use "required"))
           '()))
     
     (make-xml-element
      (xs attribute)
      #:name-value name
      #:att-list   (append l1 l2))]))

(provide xs:xs->xexpr)





(module+ test

  (require typed/rackunit)

  (: f (-> Symbol String))
  (define (f s)
    (match s
      ['string "xs:string"]
      ['blub   "z:blub"]
      ['btype  "tns:btype"]))

  (define a-element : xs:element
    (xs:element
     'value
     'string
     1
     1))

  (define x-element : XExpr
    '(xs:element
      ((name "value")
       (type "xs:string"))))

  (define a-restriction : xs:restriction
    (xs:restriction
     'string
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

  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:target-namespace"
                  (set)
                  (set))
                 f)
                (list
                 'xs:schema
                 '((name                 "schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:tns            "urn:target-namespace")
                   (targetNamespace      "urn:target-namespace")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))))

  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:target-namespace"
                  (set)
                  (set a-element))
                 f)
                (list
                 'xs:schema
                 '((name                 "schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:tns            "urn:target-namespace")
                   (targetNamespace      "urn:target-namespace")
                   (elementFormDefault   "qualified")
                   (attributeFormDefault "unqualified"))
                 x-element))

  (check-equal? (xs:xs->xexpr a-element f)
                x-element)

  (check-equal? (xs:xs->xexpr (xs:element 'blub 'string 2 3) f)
                '(xs:element ((name      "blub")
                              (type      "xs:string")
                              (minOccurs "2")
                              (maxOccurs "3"))))

  (check-equal? (xs:xs->xexpr (xs:simple-type 'atype a-restriction) f)
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string")))))
  
  (check-equal? (xs:xs->xexpr (xs:simple-type 'atype (xs:restriction 'string (set (xs:enumeration "bla")))) f)
                '(xs:simpleType ((name "atype"))
                                (xs:restriction ((base "xs:string"))
                                                (xs:enumeration ((value "bla"))))))

  (check-equal? (xs:xs->xexpr (xs:min-inclusive 5) f)
                '(xs:minInclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:min-exclusive 5) f)
                '(xs:minExclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-inclusive 5) f)
                '(xs:maxInclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-exclusive 5) f)
                '(xs:maxExclusive ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:enumeration "blub") f)
                '(xs:enumeration ((value "blub"))))

  (check-equal? (xs:xs->xexpr (xs:pattern "blub") f)
                '(xs:pattern ((value "blub"))))

  (check-equal? (xs:xs->xexpr (xs:length 5) f)
                '(xs:length ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:min-length 5) f)
                '(xs:minLength ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:max-length 5) f)
                '(xs:maxLength ((value "5"))))

  (check-equal? (xs:xs->xexpr (xs:complex-type 'atype (set) #f) f)
                '(xs:complexType ((name "atype"))))

  (check-equal? (xs:xs->xexpr (xs:complex-type 'atype (set) (xs:all (set))) f)
                '(xs:complexType ((name "atype")) (xs:all ())))

  (check-equal? (xs:xs->xexpr
                 (xs:complex-type 'atype (set (xs:attribute 'prodid 'string #f)) (xs:all (set)))
                 f)
                '(xs:complexType ((name "atype"))
                                 (xs:attribute ((name "prodid")
                                                (type "xs:string")))
                                 (xs:all ())))

  (check-equal? (xs:xs->xexpr (xs:attribute 'prodid 'string #f) f)
                '(xs:attribute ((name "prodid")
                                (type "xs:string"))))

  (check-equal? (xs:xs->xexpr (xs:attribute 'prodid 'string #t) f)
                '(xs:attribute ((name    "prodid")
                                (type    "xs:string")
                                (use     "required"))))

  (check-equal? (xs:xs->xexpr (xs:all (set)) f)
                '(xs:all ()))

  (check-equal? (xs:xs->xexpr (xs:all (set a-element)) f)
                (list 'xs:all '() x-element))

  (check-equal? (xs:xs->xexpr (xs:choice 1 1 (set)) f)
                '(xs:choice ()))

  (check-equal? (xs:xs->xexpr (xs:choice 2 3 (set a-element)) f)
                (list 'xs:choice '((minOccurs "2") (maxOccurs "3")) x-element))

  (check-equal? (xs:xs->xexpr
                 (xs:schema
                  'schema
                  "urn:schema"
                  (set
                   (xs:import 'z "urn:z" (set 'blub) (set)))
                  (set
                   (xs:simple-type
                    'atype
                    (xs:restriction
                     'blub
                     (set)))))
                 f)
                '(xs:schema
                  ((name                 "schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:tns            "urn:schema")
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
                   (xs:import 'z "urn:z" (set 'blub) (set)))
                  (set
                   (xs:simple-type
                    'atype
                    (xs:restriction
                     'btype
                     (set)))
                   (xs:simple-type
                    'btype
                    (xs:restriction
                     'blub
                     (set)))))
                 f)
                '(xs:schema
                  ((name                 "schema")
                   (xmlns:xs             "http://www.w3.org/2001/XMLSchema")
                   (xmlns:tns            "urn:schema")
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
                         


