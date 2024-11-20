#lang typed/racket/base

(require
 (only-in racket/set
          in-set)
 (only-in racket/match
          match)
 "alist.rkt"
 "xs.rkt")

(provide
 xs:collect-member-table)

;; xs:collect-member-table
;;------------------------------------------------------------

(: xs:collect-member-table (-> (U xs:schema-member xs:qname)
                               (Listof (Pairof Symbol xs:schema-member))))
(define (xs:collect-member-table o)

  (: proc (-> (Setof xs:element)
              (Listof (Pairof Symbol xs:schema-member))))
  (define (proc element-set)
    (for/fold ([result : (Listof (Pairof Symbol xs:schema-member))
                       '()])
              ([e : xs:element
                  (in-set element-set)])
      (match e
        [(xs:element _name type _min-occurs _max-occurs)
         (alist-union result
                      (xs:collect-member-table type))])))

  (match o

    [(xs:element name type _min-occurs _max-occurs)
     (alist-set
      (xs:collect-member-table type)
      name
      o)]

    [(xs:simple-type name (xs:restriction base _body))
     (alist-set
      (xs:collect-member-table base)
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
            (alist-union result
                         (xs:collect-member-table type))])))

     (define m2 : (Listof (Pairof Symbol xs:schema-member))
       (match body

         [#f
          '()]

         [(xs:restriction base _body)
          (xs:collect-member-table base)]

         [(xs:all element-set)
          (proc element-set)]

         [(xs:choice _min-occurs _max-occurs element-set)
          (proc element-set)]))

     (alist-set
      (alist-union m1 m2)
      name
      o)]

    [(xs:qname _source _name)
     '()]))



(module+ test

  (require
   (only-in typed/rackunit
            check-equal?)
   (only-in racket/set
            set)
   "ns.rkt")

  (let ([elem : xs:element
              (xs:element 'elem (xs string) 1 1)])
    (check-equal?
     (xs:collect-member-table elem)
     (list (cons 'elem elem))))

  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [elem : xs:element
               (xs:element 'elem t1 1 1)])
    (check-equal?
     (xs:collect-member-table elem)
     (list
      (cons 'elem elem)
      (cons 't1 t1))))

  (let ([t1 : xs:simple-type
            (xs:simple-type 't1 (xs:restriction (xs string) (set)))])
    (check-equal?
     (xs:collect-member-table t1)
     (list (cons 't1 t1))))

  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [t2 : xs:simple-type
             (xs:simple-type 't2 (xs:restriction t1 (set)))])
    (check-equal?
     (xs:collect-member-table t2)
     (list
      (cons 't2 t2)
      (cons 't1 t1))))

  (let ([t1 : xs:complex-type
            (xs:complex-type 't1 (set) #f)])
    (check-equal?
     (xs:collect-member-table t1)
     (list (cons 't1 t1))))

  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [t2 : xs:complex-type
             (xs:complex-type 't2 (set (xs:attribute 'a1 t1 #f)) #f)])
    (check-equal?
     (xs:collect-member-table t2)
     (list
      (cons 't2 t2)
      (cons 't1 t1))))

  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [t2 : xs:complex-type
             (xs:complex-type 't2 (set) (xs:restriction t1 (set)))])
    (check-equal?
     (xs:collect-member-table t2)
     (list
      (cons 't2 t2)
      (cons 't1 t1))))
             
  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [elem : xs:element
               (xs:element 'elem t1 1 1)]
         [t2 : xs:complex-type
             (xs:complex-type 't2 (set) (xs:all (set elem)))])
    (check-equal?
     (xs:collect-member-table t2)
     (list
      (cons 't2 t2)
      (cons 't1 t1))))
             
  (let* ([t1 : xs:simple-type
             (xs:simple-type 't1 (xs:restriction (xs string) (set)))]
         [elem : xs:element
               (xs:element 'elem t1 1 1)]
         [t2 : xs:complex-type
             (xs:complex-type 't2 (set) (xs:choice 1 1 (set elem)))])
    (check-equal?
     (xs:collect-member-table t2)
     (list
      (cons 't2 t2)
      (cons 't1 t1))))

  (check-equal?
   (xs:collect-member-table (xs string))
   '()))

