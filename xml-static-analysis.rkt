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
          list->set
          set
          set-member?
          set-union)
 (only-in racket/string
          string-join)
 "xml-aux.rkt"
 "xml-schema.rkt"
 "xml-wsdl.rkt")

(provide
 with-stack-entry
 stack-entry
 hash-combine
 get-simple-provide-set
 get-complex-provide-set
 get-provide-set
 get-provide-table
 verify-occur
 verify-bind)

(struct stack-entry
  ([form : (U #f Symbol)]
   [name : (U #f Symbol)]))

(define error-stack : (Parameterof (Listof stack-entry))
  (make-parameter '()))

(: format-error (-> String Any * String))
(define (format-error s . a)

  (define str-list : (Listof String)
    (for/list ([e : stack-entry
                  (in-list (error-stack))])
      (match e
        [(stack-entry form #f)
         (format "  (~a ...)" form)]
        [(stack-entry #f name)
         (format "    [~a ...]" name)]
        [(stack-entry form name)
         (format "  (~a ~a ...)" form name)])))

  (define full-list : (Listof String)
    (reverse
     (cons (apply format s a)
           str-list)))

  (string-join full-list "\n"))

(define-syntax (with-stack-entry stx)
  (syntax-parse stx
    [(_ entry e ...)
     #'(parameterize ([error-stack (cons entry (error-stack))])
         e ...)]))

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

(: get-provide-set (-> (U xs:import xs:schema)
                       (Setof Symbol)))
(define/match (get-provide-set o)

  [((xs:schema _target-namespace _import-list body))

   (: skip-element (-> Symbol Boolean))
   (define (skip-element x)
     (not (xs:element? (hash-ref body x))))

   (list->set
    (filter skip-element (hash-keys body)))]

  [((xs:import _namespace simple-provide-set complex-provide-set))
   (set-union simple-provide-set complex-provide-set)])

(: get-provide-table (-> (U xs:schema wsdl:definitions)
                         (-> (U xs:import xs:schema) (Setof Symbol))
                         (HashTable Symbol (Setof Symbol))))
(define (get-provide-table o accessor)

  (: collect (-> (HashTable Symbol (Setof Symbol))
                 (HashTable Symbol (U xs:import xs:schema))
                 (HashTable Symbol (Setof Symbol))))
  (define (collect table0 import-table)
    (for/fold ([result : (HashTable Symbol (Setof Symbol))
                       table0])
               ([prefix : Symbol
                        (in-list (hash-keys import-table))])
       (hash-set result
                 prefix
                 (accessor
                  (hash-ref import-table prefix)))))
  
  (match o

    [(xs:schema _target-namespace import-table _body)
     (collect (hash (tns-prefix) (accessor o))
              import-table)]

    [(wsdl:definitions _target-namespace import-table _body)
     (collect (hash)
              import-table)]))

(: verify-occur-range (-> Real
                          (U #f Real)
                          Void))
(define (verify-occur-range min-occurs max-occurs)
  (unless (or (not max-occurs)
               (<= min-occurs max-occurs))
    (raise-user-error (format-error
                       "invalid occurrence range [~a, ~a]"
                       min-occurs
                       max-occurs))))

(: verify-occur (->(U xs:element xs:choice)
                    Void))
(define (verify-occur o)
  (match o
    
    [(xs:element _type min-occurs max-occurs)
     (verify-occur-range min-occurs max-occurs)]

    [(xs:choice min-occurs max-occurs _body)
     (verify-occur-range min-occurs max-occurs)]))

(: verify-bind-name (-> (HashTable Symbol (Setof Symbol))
                        (-> qname)
                        Void))
(define (verify-bind-name bind-table qn)

  (: default-thunk (-> (Setof Symbol)))
  (define (default-thunk)
    (set))
  
  (match (qn)
    [(qname type-prefix type-name)
     (define relevant-set : (Setof Symbol)
       (hash-ref bind-table type-prefix default-thunk))
     (unless (set-member? relevant-set type-name)
       (raise-user-error (format-error
                          "(: ~a ~a) undefined"
                          type-prefix
                          type-name)))]))

(: verify-bind (-> (HashTable Symbol (Setof Symbol))
                   (U xs:element
                      xs:restriction
                      xs:attribute
                      wsdl:part
                      wsdl:operation)
                   Void))
(define (verify-bind bind-table o)

  (match o
    [(xs:element type _min-occurs _max-occurs)
     (verify-bind-name bind-table type)]

    [(xs:restriction base _body)
     (verify-bind-name bind-table base)]

    [(xs:attribute type _required)
     (verify-bind-name bind-table type)]

    [(wsdl:part type)
     (verify-bind-name bind-table type)]

    [(wsdl:operation input output fault)
     (when input
       (with-stack-entry (stack-entry 'input #f)
         (verify-bind-name bind-table input)))
     (when output
       (with-stack-entry (stack-entry 'output #f)
         (verify-bind-name bind-table output)))
     (when fault
       (with-stack-entry (stack-entry 'fault #f)
         (verify-bind-name bind-table fault)))]))


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

  (check-equal? (get-provide-table
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla (xs:element xs:string 1 1)
                                  'blub (xs:simple-type (xs:restriction xs:string '()))
                                  'foo (xs:complex-type (hash) (xs:all (hash)))))
                 get-simple-provide-set)
                (hash (tns-prefix) (set 'blub)))
  
  (check-equal? (get-provide-table
                 (xs:schema "urn:target-namespace"
                            (hash)
                            (hash 'bla (xs:element xs:string 1 1)
                                  'blub (xs:simple-type (xs:restriction xs:string '()))
                                  'foo (xs:complex-type (hash) (xs:all (hash)))))
                 get-complex-provide-set)
                (hash (tns-prefix) (set 'foo)))
  
  (check-equal? (get-provide-table
                 (xs:schema "urn:target-namespace"
                            (hash
                             'types (xs:import
                                     "urn:default"
                                     (set 'blub)
                                     (set 'foo)))
                            (hash))
                 get-simple-provide-set)
                (hash (tns-prefix) (set) 'types (set 'blub)))

  (check-equal? (get-provide-table
                 (xs:schema "urn:target-namespace"
                            (hash
                             'types (xs:import
                                     "urn:default"
                                     (set 'blub)
                                     (set 'foo)))
                            (hash))
                 get-simple-provide-set)
                (hash (tns-prefix) (set) 'types (set 'blub)))

  (check-equal? (get-provide-table
                 (xs:schema "urn:target-namespace"
                            (hash
                             'types (xs:import
                                     "urn:default"
                                     (set 'blub)
                                     (set 'foo)))
                            (hash))
                 get-complex-provide-set)
                (hash (tns-prefix) (set) 'types (set 'foo))))
