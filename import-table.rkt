#lang typed/racket/base

(require
 (only-in racket/match
          match)
 (only-in racket/set
          set
          set-add)
 "xs.rkt"
 "wsdl.rkt"
 "alist.rkt")

(provide
 xml:import-capable
 xml:import-capable?
 xml:collect-import-table)

;; xml:import-capable
;;------------------------------------------------------------

(define-type xml:import-capable
  (U xs:schema
     xs:element
     xs:simple-type
     xs:complex-type
     xs:attribute
     False
     xs:all
     xs:choice
     xs:restriction
     xs:qname
     wsdl:definitions
     wsdl:message
     wsdl:part
     wsdl:port-type
     wsdl:operation))

(define-predicate xml:import-capable?
  xml:import-capable)


;; xml:collect-import-table
;;------------------------------------------------------------

(: xml:collect-import-table (-> xml:import-capable
                           (Alistof String)))
(define (xml:collect-import-table o)

  (: proc (-> (Setof xml:import-capable) (Alistof String)))
  (define (proc s)
    (alist-apply-union xml:collect-import-table s))

  (match o
    [(xs:schema _name _namespace body)
     (proc body)]
  
    [(xs:element _name type _min-occurs _max-occurs)
     (xml:collect-import-table type)]

    [(xs:simple-type _name restriction)
     (xml:collect-import-table restriction)]

    [(xs:complex-type _name attribute-set body)
     (proc (set-add attribute-set body))]

    [(xs:attribute _name type _required)
     (xml:collect-import-table type)]

    [#f
     '()]

    [(xs:all element-set)
     (proc element-set)]

    [(xs:choice _min-occurs _max-occurs element-set)
     (proc element-set)]

    [(xs:restriction base _body)
     (xml:collect-import-table base)]

    [(xs:qname (xs:import name namespace _simple-provide-set _complex-provide-set) _ref)
        (list (cons name namespace))]

    [(wsdl:definitions _name _namespace body)
     (proc body)]

    [(wsdl:message _name part-set)
     (proc part-set)]

    [(wsdl:part _name type)
     (xml:collect-import-table type)]

    [(wsdl:port-type _name operation-set)
     (proc operation-set)]

    [(wsdl:operation _name input output fault)
     (alist-apply-union
      xml:collect-import-table
      (set input output fault))]))



(module+ test

  (require
   (only-in typed/rackunit
            check-equal?)
   "ns.rkt")

  (check-equal? (xml:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set)))
                '())

  (check-equal? (xml:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs string) 1 1))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs:qname (xs:import 'a "urn:a" (set) (set)) 'x) 1 1)
                                 (xs:element 'blub (xs:qname (xs:import 'b "urn:b" (set) (set)) 'y) 1 1))))
                '((a . "urn:a")
                  (b . "urn:b")))

  (check-equal? (xml:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs:qname (xs:import 'a "urn:a" (set) (set)) 'x) 1 1)
                                 (xs:element 'blub (xs:qname (xs:import 'a "urn:a" (set) (set)) 'y) 1 1))))
                '((a . "urn:a")))

  (check-equal? (xml:collect-import-table
                 (xs:element 'bla (xs string) 1 1))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:simple-type
                  'a
                  (xs:restriction (xs string) (set))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  #f))
                '())

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set (xs:attribute 'a (xs string) #f))
                  #f))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set (xs:attribute 'a (xs:qname (xs:import 'bla "urn:bla" (set) (set)) 'r) #f)
                       (xs:attribute 'b (xs:qname (xs:import 'blub "urn:blub" (set) (set)) 's) #f))
                  #f))
                '((bla . "urn:bla")
                  (blub . "urn:blub")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set))))
                '())

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set (xs:element 'a (xs string) 1 1)))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set (xs:element 'a (xs:qname (xs:import 'bla "urn:bla" (set) (set)) 'r) 1 1)
                               (xs:element 'b (xs:qname (xs:import 'blub "urn:blub" (set) (set)) 'r) 1 1)))))
                '((bla . "urn:bla")
                  (blub ."urn:blub")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:choice
                   1
                   1
                   (set))))
                '())

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:choice
                   1
                   1
                   (set (xs:element 'a (xs string) 1 1)))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:choice
                   1
                   1
                   (set (xs:element 'a (xs:qname (xs:import 'bla "urn:bla" (set) (set)) 'r) 1 1)
                        (xs:element 'b (xs:qname (xs:import 'blub "urn:blub" (set) (set)) 'r) 1 1)))))
                '((bla . "urn:bla")
                  (blub . "urn:blub")))

  (check-equal? (xml:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:restriction (xs string) (set))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))
  
  (check-equal? (xml:collect-import-table
                 (xs:restriction (xs string) (set)))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))
  
  (check-equal? (xml:collect-import-table
                 (xs:qname (xs:import 'bla "urn:bla" (set) (set))
                           'blub))
                '((bla ."urn:bla"))))


