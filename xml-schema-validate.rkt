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
 "xml-schema.rkt"
 "xml-static-analysis.rkt")

(provide
 validate-xs:schema)


(: verify-xs:complex-type-member (-> (HashTable Symbol (Setof Symbol))
                                     (HashTable Symbol (Setof Symbol))
                                     xs:complex-type-member
                                     Void))
(define (verify-xs:complex-type-member simple-table complex-table o)

  (: proc (-> Symbol xs:element Void))
  (define (proc name elem)
    (with-stack-entry (stack-entry #f name)
      (verify-xs:schema-member
       simple-table
       complex-table
       elem)))
  
  (match o

    [#f
     (void)]

    [(xs:restriction _base _body)
     (verify-bind simple-table o)]

    [(xs:all element-table)
     (with-stack-entry (stack-entry 'all #f)
       (hash-for-each element-table proc))]

    [(xs:choice _min-occurs _max-occurs element-table)
     (with-stack-entry (stack-entry 'choice #f)
       (verify-occur o)
       (hash-for-each element-table proc))]))

(: verify-xs:schema-member (-> (HashTable Symbol (Setof Symbol))
                               (HashTable Symbol (Setof Symbol))
                               xs:schema-member
                               Void))
(define (verify-xs:schema-member simple-table complex-table o)

  (: proc (-> Symbol xs:attribute Void))
  (define (proc att-name att)
    (match att
      [(xs:attribute type _required)
       (with-stack-entry (stack-entry #f att-name)
         (verify-bind simple-table att))]))

  (match o
    
    [(xs:element type min-occurs max-occurs)
     (verify-occur o)
     (verify-bind (hash-combine simple-table complex-table) o)]

    [(xs:simple-type restriction)
     (verify-bind simple-table restriction)]
    
    [(xs:complex-type attribute-table body)
     (hash-for-each attribute-table proc)
     (verify-xs:complex-type-member simple-table complex-table body)]))


(: verify-xs:schema (-> xs:schema
                        Void))
(define (verify-xs:schema schema)

  (define simple-table : (HashTable Symbol (Setof Symbol))
    (get-provide-table schema get-simple-provide-set))

  (define complex-table : (HashTable Symbol (Setof Symbol))
    (get-provide-table schema get-complex-provide-set))

  (: proc (-> Symbol
              xs:schema-member
              Void))
  (define (proc x o)
    (match o
      [(xs:element _type _min-occurs _max-occurs)
       (with-stack-entry (stack-entry 'define-element x)
         (verify-xs:schema-member
          simple-table
          complex-table
          o))]
      [_
       (with-stack-entry (stack-entry 'define-type x)
         (verify-xs:schema-member
          simple-table
          complex-table
          o))]))

  (match schema
    [(xs:schema _target-namespace import-table body)
     (hash-for-each body proc)]))

(: validate-xs:schema (-> xs:schema
                          xs:schema))
(define (validate-xs:schema schema)
  (verify-xs:schema schema)
  schema)

(module+ test

  (require typed/rackunit)

  (check-not-exn
   (lambda ()
     (verify-xs:schema-member
      (hash 'tns (set 'myType))
      (hash)
      (xs:element (tns myType) 1 1))))

  (check-not-exn
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash 'tns (set 'myType))
      (xs:element (tns myType) 1 1))))

  (check-exn
   exn:fail:user?
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash)
      (xs:element (tns myType) 1 1))))

  (check-exn
   exn:fail:user?
   (lambda ()
     (verify-xs:schema-member
      (hash)
      (hash 'tns (set 'myType))
      (xs:element (tns myType) 1 0)))))  
