#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           id))
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          in-set
          set
          set-add)
 "xml-schema.rkt"
 "xml-wsdl.rkt")

;; constants
;;------------------------------------------------------------

(define xml:prefix-tns : Symbol
  'tns)

(define xml:prefix-xs : Symbol
  'xs)

(define xml:prefix-wsdl : Symbol
  'wsdl)

(define xml:prefix-soap : Symbol
  'soap)
  
(define xml:prefix-table : (Immutable-HashTable Symbol String)
  (hash xml:prefix-xs   "http://www.w3.org/2001/XMLSchema"
        xml:prefix-wsdl "http://schemas.xmlsoap.org/wsdl"
        xml:prefix-soap "http://schemas.xmlsoap.org/wsdl/soap"))
        
(provide xml:prefix-tns
         xml:prefix-xs
         xml:prefix-wsdl
         xml:prefix-soap
         xml:prefix-table)
        

;; qname
;;------------------------------------------------------------

(: xs:qname->string (-> xs:qname String))
(define/match (xs:qname->string qn)
  [((xs:qname (xs:import prefix
                         _namespace
                         _simple-provide-set
                         _complex-provide-set)
              name))
   (format "~a:~a" prefix name)]
  [((xs:qname (xs:schema prefix
                         _namespace
                         _body)
              name))
   (format "~a:~a" prefix name)])


(: xs:qname->symbol (-> xs:qname Symbol))
(define (xs:qname->symbol qn)
  (string->symbol (xs:qname->string qn)))

(provide xs:qname->string
         xs:qname->symbol)


;; language extensions
;;------------------------------------------------------------

(define xs:xs-simple-provide-set : (Setof Symbol)
  (set 'boolean
       'date
       'dateTime
       'decimal
       'integer
       'nonNegativeInteger
       'NMTOKEN
       'string
       'token
       'unsignedShort))

(: xs:make-xs (-> Symbol xs:qname))
(define (xs:make-xs ref)

  (xs:qname
   (xs:import
    xml:prefix-xs
    (hash-ref xml:prefix-table xml:prefix-xs)
    xs:xs-simple-provide-set
    (set))
   ref))

(define-syntax (xs stx)
  (syntax-parse stx
      [(_ x:id)
       #'(xs:make-xs 'x)]))

(provide xs:xs-simple-provide-set
         xs:make-xs
         xs)


;; xml:get-name
;;------------------------------------------------------------

(define-type xml:nameable
  (U xs:schema
     xs:import
     xs:element
     xs:simple-type
     xs:complex-type
     xs:attribute))

(define-predicate xml:nameable?
  xml:nameable)

(: xml:get-name (-> xml:nameable Symbol))
(define/match (xml:get-name _o)

  [((xs:schema name _namespace _body))
   name]

  [((xs:import name _namespace _simple-provide-set _complex-provide-set))
   name]

  [((xs:element name _type _min-occurs _max-occurs))
   name]

  [((xs:simple-type name _restriction))
   name]

  [((xs:complex-type name _attribute-set _body))
   name]

  [((xs:attribute name _type _required))
   name])

(provide
 xml:nameable
 xml:nameable?
 xml:get-name)

;; xml:alist-union
;;------------------------------------------------------------

(: xml:alist-union (All (b)
                       (-> (Listof (Pairof Symbol b)) *
                           (Listof (Pairof Symbol b)))))
(define (xml:alist-union . alist-list)
  (sort
   (for/fold ([r : (Listof (Pairof Symbol b))
                 '()])
             ([alist : (Listof (Pairof Symbol b))
                     (in-list alist-list)])
     (for/fold ([result : (Listof (Pairof Symbol b))
                        r])
               ([p : (Pairof Symbol b)
                   (in-list alist)])
       (match p
         [(cons k v)
          (if (assoc k result)
              result
              (cons (cons k v) result))])))
   (lambda ([x : (Pairof Symbol b)]
            [y : (Pairof Symbol b)])
     (symbol<? (car x) (car y)))))

(: xml:alist-add (All (b)
                      (-> (Listof (Pairof Symbol b))
                          Symbol
                          b
                          (Listof (Pairof Symbol b)))))
(define (xml:alist-add alist x v)
  (if (assoc x alist)
      alist
      (cons
       (cons x v)
       alist)))

(: xml:alist-apply-union (All (a b) (-> (-> a (Listof (Pairof Symbol b)))
                                        (Setof a)
                                        (Listof (Pairof Symbol b)))))
(define (xml:alist-apply-union f s)

  (define l : (Listof (Listof (Pairof Symbol b)))
    (for/list ([x : a
                  (in-set s)])
      (f x)))
    
  (apply xml:alist-union l))

(provide xml:alist-union
         xml:alist-add
         xml:alist-apply-union)


;; xs:type->string
;;------------------------------------------------------------

(: xs:type->string (-> (U xs:qname xs:simple-type xs:complex-type wsdl:message)
                       String))
(define/match (xs:type->string o)

  ;; xs:qname
  [((xs:qname source ref))
   (xs:qname->string o)]

  ;; xs:simple-type
  [((xs:simple-type name _restriction))
   (format "~a:~a" xml:prefix-tns name)]

  ;; xs:complex-type
  [((xs:complex-type name _attribute-set _body))
   (format "~a:~a" xml:prefix-tns name)]

  ;; wsdl:message
  [((wsdl:message name _part-set))
   (format "~a:~a" xml:prefix-tns name)])

(provide
 xs:type->string)


;; xml:collect-member-table
;;------------------------------------------------------------

(: xml:collect-member-table (-> (U xs:qname xs:element xs:simple-type xs:complex-type)
                               (Listof (Pairof Symbol xs:schema-member))))
(define (xml:collect-member-table o)

  (: proc (-> (Setof xs:element)
              (Listof (Pairof Symbol xs:schema-member))))
  (define (proc element-set)
    (for/fold ([result : (Listof (Pairof Symbol xs:schema-member))
                       '()])
              ([e : xs:element
                  (in-set element-set)])
      (match e
        [(xs:element _name type _min-occurs _max-occurs)
         (xml:alist-union result
                          (xml:collect-member-table type))])))

  (match o

    [(xs:qname _source _name)
     '()]

    [(xs:element name type _min-occurs _max-occurs)
     (xml:alist-add
      (xml:collect-member-table type)
      name
      o)]

    [(xs:simple-type name (xs:restriction base _body))
     (xml:alist-add
      (xml:collect-member-table base)
      name
      o)]

    [(xs:complex-type name attribute-set body)

     (define m1 : (Listof (Pairof Symbol xs:schema-member))
       (for/fold ([result : (Listof (Pairof Symbol xs:schema-member))
                          '()])
                 ([a : xs:attribute
                     (in-set attribute-set)])
         (match a
           [(xs:attribute _name type _required)
            (xml:alist-union result
                             (xml:collect-member-table type))])))

     (define m2 : (Listof (Pairof Symbol xs:schema-member))
       (match body

         [#f
          '()]

         [(xs:restriction base _body)
          (xml:collect-member-table base)]

         [(xs:all element-set)
          (proc element-set)]

         [(xs:choice _min-occurs _max-occurs element-set)
          (proc element-set)]))

     (xml:alist-union m1 m2)]))
          
(provide xml:collect-member-table)


;; xs:import-capable
;;------------------------------------------------------------

(define-type xs:import-capable
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

(define-predicate xs:import-capable?
  xs:import-capable)

(provide xs:import-capable
         xs:import-capable?)

;; xs:collect-import-table
;;------------------------------------------------------------

(: xs:collect-import-table (-> xs:import-capable
                           (Listof (Pairof Symbol String))))
(define (xs:collect-import-table o)

  (: proc (-> (Setof xs:import-capable) (Listof (Pairof Symbol String))))
  (define (proc s)
    (xml:alist-apply-union xs:collect-import-table s))

  (match o
    [(xs:schema _name _namespace body)
     (proc body)]
  
    [(xs:element _name type _min-occurs _max-occurs)
     (xs:collect-import-table type)]

    [(xs:simple-type _name restriction)
     (xs:collect-import-table restriction)]

    [(xs:complex-type _name attribute-set body)
     (proc (set-add attribute-set body))]

    [(xs:attribute _name type _required)
     (xs:collect-import-table type)]

    [#f
     '()]

    [(xs:all element-set)
     (proc element-set)]

    [(xs:choice _min-occurs _max-occurs element-set)
     (proc element-set)]

    [(xs:restriction base _body)
     (xs:collect-import-table base)]

    [(xs:qname source _ref)
     (match source

       [(xs:import name namespace _simple-provide-set _complex-provide-set)
        (list (cons name namespace))]

       [(xs:schema name namespace _body)
        (list (cons name namespace))])]

    [(wsdl:definitions _namespace body)
     (proc body)]

    [(wsdl:message _name part-set)
     (proc part-set)]

    [(wsdl:part _name type)
     (xs:collect-import-table type)]

    [(wsdl:port-type _name operation-set)
     (proc operation-set)]

    [(wsdl:operation _name input output fault)
     (xml:alist-apply-union
      xs:collect-import-table
      (set input output fault))]))

(provide xs:collect-import-table)


;; xs:extend-import-table
;;------------------------------------------------------------

(: xs:extend-import-table (-> (Listof (Pairof Symbol String))
                              String
                              Symbol *
                              (Listof (Pairof Symbol String))))
(define (xs:extend-import-table import-table tns . sym-list)

  (: extend (-> (Listof (Pairof Symbol String))
                Symbol
                (Listof (Pairof Symbol String))))
  (define (extend t s)
    (if (assoc s t)
        t
        (let ([pair : (Pairof Symbol String)
                    (cons
                     s
                     (hash-ref
                      xml:prefix-table
                      s
                      (lambda ()
                        (raise-user-error 'xs:extend-import-table "namespace prefix undefined: " s))))])
          (cons pair t))))

  (match sym-list
    ['()
     (cons
      (cons xml:prefix-tns tns)
      import-table)]

    [(cons hd tl)
     (apply xs:extend-import-table
            (extend import-table hd)
            tns
            tl)]))
     
(provide xs:extend-import-table)


(module+ test

  (require typed/rackunit)

  ;; qname
  ;;------------------------------------------------------------

  (check-equal? (xs:qname->string
                 (xs:qname
                  (xs:import 'bla
                             "http://bla.org"
                             (set)
                             (set))
                  'blub))
                "bla:blub")

  (check-equal? (xs:qname->symbol
                 (xs:qname
                  (xs:import 'bla
                             "http://bla.org"
                             (set)
                             (set))
                  'blub))
                'bla:blub)

  (check-equal? (xs:qname->string
                 (xs:qname
                  (xs:schema 'bla
                             "http://bla.org"
                             (set))
                  'blub))
                "bla:blub")

  (check-equal? (xs:qname->symbol
                 (xs:qname
                  (xs:schema 'bla
                             "http://bla.org"
                             (set))
                  'blub))
                'bla:blub)

(check-equal? (xs:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set)))
                '())

  (check-equal? (xs:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs string) 1 1))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs:qname (xs:import 'a "urn:a" (set) (set)) 'x) 1 1)
                                 (xs:element 'blub (xs:qname (xs:import 'b "urn:b" (set) (set)) 'y) 1 1))))
                '((a . "urn:a")
                  (b . "urn:b")))

  (check-equal? (xs:collect-import-table
                 (xs:schema 'schema
                            "urn:target-namespace"
                            (set (xs:element 'bla (xs:qname (xs:import 'a "urn:a" (set) (set)) 'x) 1 1)
                                 (xs:element 'blub (xs:qname (xs:import 'a "urn:a" (set) (set)) 'y) 1 1))))
                '((a . "urn:a")))

  (check-equal? (xs:collect-import-table
                 (xs:element 'bla (xs string) 1 1))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
                 (xs:simple-type
                  'a
                  (xs:restriction (xs string) (set))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  #f))
                '())

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set (xs:attribute 'a (xs string) #f))
                  #f))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set (xs:attribute 'a (xs:qname (xs:import 'bla "urn:bla" (set) (set)) 'r) #f)
                       (xs:attribute 'b (xs:qname (xs:import 'blub "urn:blub" (set) (set)) 's) #f))
                  #f))
                '((bla . "urn:bla")
                  (blub . "urn:blub")))

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set))))
                '())

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set (xs:element 'a (xs string) 1 1)))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:all (set (xs:element 'a (xs:qname (xs:import 'bla "urn:bla" (set) (set)) 'r) 1 1)
                               (xs:element 'b (xs:qname (xs:import 'blub "urn:blub" (set) (set)) 'r) 1 1)))))
                '((bla . "urn:bla")
                  (blub ."urn:blub")))

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:choice
                   1
                   1
                   (set))))
                '())

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:choice
                   1
                   1
                   (set (xs:element 'a (xs string) 1 1)))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))

  (check-equal? (xs:collect-import-table
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

  (check-equal? (xs:collect-import-table
                 (xs:complex-type
                  'b
                  (set)
                  (xs:restriction (xs string) (set))))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))
  
  (check-equal? (xs:collect-import-table
                 (xs:restriction (xs string) (set)))
                (list (cons 'xs "http://www.w3.org/2001/XMLSchema")))
  
  (check-equal? (xs:collect-import-table
                 (xs:qname (xs:import 'bla "urn:bla" (set) (set))
                           'blub))
                '((bla ."urn:bla")))

  (check-equal? (xs:collect-import-table
                 (xs:qname (xs:schema 'bla "urn:bla" (set))
                           'blub))
                '((bla ."urn:bla")))

  )
