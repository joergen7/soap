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
     (verify-bind simple-table loc o)]

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
         (verify-bind simple-table loc att)])))

  (match o
    
    [(xs:element type min-occurs max-occurs)
     (let ([loc : String
                (format "element ~a" name)])
       (verify-occur loc o)
       (verify-bind (hash-combine simple-table complex-table) loc o))]

    [(xs:simple-type body)
     (let ([loc : String
                (format "type ~a" name)])
       (verify-bind simple-table loc body))]
    
    [(xs:complex-type attribute-table body)
     (hash-for-each attribute-table proc)
     (verify-xs:complex-type-member simple-table complex-table name body)]))


(: verify-xs:schema (-> xs:schema
                        Void))
(define (verify-xs:schema schema)

  (define simple-table : (HashTable Symbol (Setof Symbol))
    (get-provide-table schema get-simple-provide-set))

  (define complex-table : (HashTable Symbol (Setof Symbol))
    (get-provide-table schema get-complex-provide-set))

  (: ver (-> Symbol
             xs:schema-member
             Void))
  (define (ver x o)
    (verify-xs:schema-member
     simple-table
     complex-table
     x
     o))

  (match schema
    [(xs:schema _target-namespace import-table body)
     (hash-for-each body ver)]))

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
