#lang typed/racket/base

(require
 (only-in racket/match
          match)
 (only-in racket/set
          set
          set-union)
 "alist.rkt"
 "wsdl.rkt")

(provide
 wsdl:collect-member-table)

;; wsdl:collect-member-table
;;------------------------------------------------------------

(: wsdl:collect-member-table (-> (U wsdl:definitions-member wsdl:operation)
                                 (Alistof wsdl:definitions-member)))
(define (wsdl:collect-member-table o)

  (match o

    [(wsdl:message name _part-set)
     (list (cons name o))]

    [(wsdl:port-type name operation-set)
     (alist-set
      (alist-apply-union
       wsdl:collect-member-table
       operation-set)
      name
      o)]

    [(wsdl:operation name input output fault)

     (define a : (Setof wsdl:message)
       (if input
           (set input)
           (set)))

     (define b : (Setof wsdl:message)
       (if output
           (set output)
           (set)))

     (define c : (Setof wsdl:message)
       (if fault
           (set fault)
           (set)))
       
     (define s : (Setof wsdl:message)
       (set-union a b c))

     (alist-apply-union
      wsdl:collect-member-table
      s)]))

(module+ test

  (require
   (only-in typed/rackunit
            check-equal?)
   "ns-forms.rkt")

  (let* ([m : wsdl:message
            (wsdl:message 'm (set (wsdl:part 'body (xs string))))])
    (check-equal?
     (wsdl:collect-member-table m)
     (list (cons 'm m))))

  (let ([pt : wsdl:port-type
            (wsdl:port-type 'pt (set))])
    (check-equal?
     (wsdl:collect-member-table pt)
     (list (cons 'pt pt))))

  (let* ([op : wsdl:operation
             (wsdl:operation 'op #f #f #f)]
         [pt : wsdl:port-type
             (wsdl:port-type 'pt (set op))])
    (check-equal?
     (wsdl:collect-member-table pt)
     (list (cons 'pt pt))))


  (let* ([op1 : wsdl:operation
              (wsdl:operation 'op1 #f #f #f)]
         [op2 : wsdl:operation
              (wsdl:operation 'op2 #f #f #f)]
         [pt : wsdl:port-type
             (wsdl:port-type 'pt (set op1 op2))])
    (check-equal?
     (wsdl:collect-member-table pt)
     (list (cons 'pt pt))))



  (let ([op : wsdl:operation
            (wsdl:operation 'op #f #f #f)])
    (check-equal?
     (wsdl:collect-member-table op)
     '()))

  (let* ([m : wsdl:message
            (wsdl:message 'm (set (wsdl:part 'body (xs string))))]
         [op : wsdl:operation
             (wsdl:operation 'op1 m #f #f)])
    (check-equal?
     (wsdl:collect-member-table op)
     (list (cons 'm m))))
     
  (let* ([m : wsdl:message
            (wsdl:message 'm (set (wsdl:part 'body (xs string))))]
         [op : wsdl:operation
             (wsdl:operation 'op1 #f m #f)])
    (check-equal?
     (wsdl:collect-member-table op)
     (list (cons 'm m))))
     
  (let* ([m : wsdl:message
            (wsdl:message 'm (set (wsdl:part 'body (xs string))))]
         [op : wsdl:operation
             (wsdl:operation 'op1 #f #f m)])
    (check-equal?
     (wsdl:collect-member-table op)
     (list (cons 'm m))))
     
  (let* ([m1 : wsdl:message
             (wsdl:message 'm1 (set (wsdl:part 'body (xs string))))]
         [m2 : wsdl:message
             (wsdl:message 'm2 (set (wsdl:part 'body (xs string))))]
         [m3 : wsdl:message
             (wsdl:message 'm3 (set (wsdl:part 'body (xs string))))]
         [op : wsdl:operation
             (wsdl:operation 'op1 m1 m2 m3)])
    (check-equal?
     (wsdl:collect-member-table op)
     (list
      (cons 'm1 m1)
      (cons 'm2 m2)
      (cons 'm3 m3))))
     

  )
