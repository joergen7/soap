#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          list->set
          set
          set-member?
          set-union)
 "xml-aux.rkt"
 "xml-schema.rkt")

(provide
 validate-xs:schema)

(: hash-combine (-> (HashTable Symbol (Setof Symbol))
                    (HashTable Symbol (Setof Symbol))
                    (HashTable Symbol (Setof Symbol))))
(define (hash-combine t1 t2)

  (: default-thunk (-> (Setof Symbol)))
  (define (default-thunk)
    (set))
  
  (for/fold ([result : (HashTable Symbol (Setof Symbol))
                     (hash)])
            ([p : (Pairof Symbol (Setof Symbol))
                (in-list
                 (append (hash->list t1)
                         (hash->list t2)))])
    (match p
      [(cons x y)
       
       (define set1 : (Setof Symbol)
         (hash-ref result x default-thunk))

       (define set2 : (Setof Symbol)
         (set-union set1 y))
       
       (hash-set result x set2)])))

(: get-simple-provide-set (-> (U xs:import xs:schema)
                              (Setof Symbol)))
(define/match (get-simple-provide-set o)

  [((xs:schema _target-namespace _import-list body))

   (: select-simple (-> Symbol Boolean))
   (define (select-simple x)
     (xs:simple-type? (hash-ref body x)))

   (list->set
    (filter select-simple (hash-keys body)))]
    
  [((xs:import _namespace simple-provide-set _complex-provide-set))
   simple-provide-set])

(: get-complex-provide-set (-> (U xs:import xs:schema)
                               (Setof Symbol)))
(define/match (get-complex-provide-set o)

  [((xs:schema _target-namespace _import-list body))

   (: select-complex (-> Symbol Boolean))
   (define (select-complex x)
     (xs:complex-type? (hash-ref body x)))

   (list->set
    (filter select-complex (hash-keys body)))]
    
  [((xs:import _namespace _simple-provide-set complex-provide-set))
   complex-provide-set])


(: get-provide-table (-> xs:schema
                         (-> (U xs:import xs:schema) (Setof Symbol))
                         (HashTable Symbol (Setof Symbol))))
(define (get-provide-table schema accessor)
  (match schema
    [(xs:schema _target-namespace import-table _body)
     (for/fold ([result : (HashTable Symbol (Setof Symbol))
                        (hash (tns-prefix) (accessor schema))])
               ([prefix : Symbol
                        (in-list (hash-keys import-table))])
       (hash-set result
                 prefix
                 (get-simple-provide-set
                  (hash-ref import-table prefix))))]))

(: verify-occur-range (-> String Real (U #f Real) Void))
(define (verify-occur-range loc min-occurs max-occurs)
  (unless (or (not max-occurs)
               (<= min-occurs max-occurs))
    (raise-user-error
     (format "~a: invalid occurrence range [~a, ~a]"
             loc
             min-occurs
             max-occurs))))

(: verify-occur (-> String (U xs:element xs:choice) Void))
(define (verify-occur loc o)
  (match o
    
    [(xs:element _type min-occurs max-occurs)
     (verify-occur-range loc min-occurs max-occurs)]

    [(xs:choice min-occurs max-occurs _body)
     (verify-occur-range loc min-occurs max-occurs)]))

(: verify-bind-name (-> String (HashTable Symbol (Setof Symbol)) (-> qname) Void))
(define (verify-bind-name loc bind-table qn)

  (: default-thunk (-> (Setof Symbol)))
  (define (default-thunk)
    (set))
  
  (match (qn)
    [(qname type-prefix type-name)
     (define relevant-set : (Setof Symbol)
       (hash-ref bind-table type-prefix default-thunk))
     (unless (set-member? relevant-set type-name)
       (raise-user-error
        (format "~a: type ~a:~a undefined || relevant set: ~a"
                loc
                type-prefix
                type-name
                relevant-set)))]))

(: verify-bind (-> (HashTable Symbol (Setof Symbol))
                   (HashTable Symbol (Setof Symbol))
                   String
                   (U xs:element
                      xs:restriction
                      xs:attribute)
                   Void))
(define (verify-bind simple-table complex-table loc o)

  (match o
    [(xs:element type _min-occurs _max-occurs)
     (verify-bind-name loc
                       (hash-combine
                        simple-table
                        complex-table)
                       type)]

    [(xs:restriction base _body)
     (verify-bind-name loc
                       simple-table
                       base)]

    [(xs:attribute type _required)
     (verify-bind-name loc
                       simple-table
                       type)]))

(: verify-xs:complex-type-member (-> (HashTable Symbol (Setof Symbol))
                                     (HashTable Symbol (Setof Symbol))
                                     Symbol
                                     xs:complex-type-member
                                     Void))
(define (verify-xs:complex-type-member simple-table complex-table name o)

  (define loc : String
    (format "type ~a" name))

  (: proc (-> Symbol xs:element Void))
  (define (proc name elem)
    (verify-xs:schema-member
     simple-table
     complex-table
     name
     elem))
  
  (match o

    [#f
     (void)]

    [(xs:restriction _base _body)
     (verify-bind simple-table complex-table loc o)]

    [(xs:sequence body)
     (hash-for-each body proc)]

    [(xs:all body)
     (hash-for-each body proc)]

    [(xs:choice _min-occurs _max-occurs body)
     (verify-occur loc o)
     (hash-for-each body proc)]))

(: verify-xs:schema-member (-> (HashTable Symbol (Setof Symbol))
                               (HashTable Symbol (Setof Symbol))
                               Symbol
                               xs:schema-member
                               Void))
(define (verify-xs:schema-member simple-table complex-table name o)

  (: proc (-> Symbol xs:attribute Void))
  (define (proc att-name att)
    (let ([loc : String
               (format "type ~a/attribute ~a" name att-name)])
      (match att
        [(xs:attribute type _required)
         (verify-bind simple-table complex-table loc att)])))

  (match o
    
    [(xs:element type min-occurs max-occurs)
     (let ([loc : String
                (format "element ~a" name)])
       (verify-occur loc o)
       (verify-bind simple-table complex-table loc o))]

    [(xs:simple-type body)
     (let ([loc : String
                (format "type ~a" name)])
       (verify-bind simple-table complex-table loc body))]
    
    [(xs:complex-type attribute-table body)
     (hash-for-each attribute-table proc)
     (verify-xs:complex-type-member simple-table complex-table name body)]))


(: verify-xs:schema (-> Any
                    Void))
(define/match (verify-xs:schema schema)

  [((xs:schema _target-namespace import-table body))
   
   (define simple-table : (HashTable Symbol (Setof Symbol))
     (get-provide-table schema get-simple-provide-set))

   (define complex-table : (HashTable Symbol (Setof Symbol))
     (get-provide-table schema get-complex-provide-set))

   (: ver (-> Symbol xs:schema-member Void))
   (define (ver x o)
     (verify-xs:schema-member simple-table
                           complex-table
                           x
                           o))
   
   (hash-for-each body ver)])

(: validate-xs:schema (-> xs:schema
                          xs:schema))
(define (validate-xs:schema schema)
  (verify-xs:schema schema)
  schema)

(module+ test

  (require typed/rackunit)

  (let ([t1 : (HashTable Symbol (Setof Symbol)) (hash)]
        [t2 : (HashTable Symbol (Setof Symbol)) (hash)])
    (check-equal? (hash-combine t1 t2)
                  (hash)))

  (let ([t1 : (HashTable Symbol (Setof Symbol)) (hash 'a (set 'r))]
        [t2 : (HashTable Symbol (Setof Symbol)) (hash)])
    (check-equal? (hash-combine t1 t2)
                  (hash 'a (set 'r))))

  (let ([t1 : (HashTable Symbol (Setof Symbol)) (hash)]
        [t2 : (HashTable Symbol (Setof Symbol)) (hash 'a (set 'r))])
    (check-equal? (hash-combine t1 t2)
                  (hash 'a (set 'r))))

  (let ([t1 : (HashTable Symbol (Setof Symbol)) (hash 'a (set 'r))]
        [t2 : (HashTable Symbol (Setof Symbol)) (hash 'b (set 's))])
    (check-equal? (hash-combine t1 t2)
                  (hash 'a (set 'r)
                        'b (set 's))))

  (let ([t1 : (HashTable Symbol (Setof Symbol)) (hash 'a (set 'r))]
        [t2 : (HashTable Symbol (Setof Symbol)) (hash 'a (set 's))])
    (check-equal? (hash-combine t1 t2)
                  (hash 'a (set 'r 's))))

  (check-equal? (get-simple-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash)))
                (set))

  (check-equal? (get-complex-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash)))
                (set))

  (check-equal? (get-simple-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla (xs:element xs:string 1 1))))
                (set))

  (check-equal? (get-complex-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla (xs:element xs:string 1 1))))
                (set))

  (check-equal? (get-simple-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla  (xs:element xs:string 1 1)
                                  'blub (xs:simple-type (xs:restriction xs:string '()))
                                  'foo  (xs:complex-type (hash) (xs:all (hash))))))
                (set 'blub))

  (check-equal? (get-complex-provide-set
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla (xs:element xs:string 1 1)
                                  'blub (xs:simple-type (xs:restriction xs:string '()))
                                  'foo (xs:complex-type (hash) (xs:all (hash))))))
                (set 'foo))

  (check-not-exn
   (lambda ()
     (verify-xs:schema-member
      (hash 'tns (set 'myType))
      (hash)
      'e
      (xs:element (tns myType) 1 1))))

  (check-not-exn
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash 'tns (set 'myType))
      'e
      (xs:element (tns myType) 1 1))))

  (check-exn
   exn:fail:user?
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash)
      'e
      (xs:element (tns myType) 1 1))))

  (check-exn
   exn:fail:user?
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash 'tns (set 'myType))
      'e
      (xs:element (tns myType) 1 0))))

)  
