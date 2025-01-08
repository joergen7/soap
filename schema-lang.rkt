#lang typed/racket/base

(provide
 t-base
 t-base?
 t-record
 t-list
 t-enum
 t-range-int
 t-range-real
 t-pattern
 t-union
 tp
 tp?)

(define-type t-base
  (U 'boolean
     'string
     'natural
     'integer
     'real
     'date))

(define-predicate t-base?
  t-base)

(struct t-root
  ([name : Symbol])
  #:transparent)

(struct t-record t-root
  ([field-table-req : (Immutable-HashTable Symbol Symbol)]
   [field-table-opt : (Immutable-HashTable Symbol Symbol)])
  #:transparent)

(struct t-list t-root
  ([member-type : Symbol])
  #:transparent)

(struct t-enum t-root
  ([value-set : (Setof Symbol)])
  #:transparent)

(struct t-range-int t-root
  ([lo   : Integer]
   [hi   : Integer])
  #:transparent)

(struct t-range-real t-root
  ([lo   : Real]
   [hi   : Real])
  #:transparent)

(struct t-pattern t-root
  ([pattern : Regexp]))

(struct t-union t-root
  ([type-set : (Setof Symbol)])
  #:transparent)

(define-type tp
  (U t-base
     t-record
     t-list
     t-enum
     t-range-int
     t-range-real
     t-pattern
     t-union))

(define-predicate tp?
  tp)

