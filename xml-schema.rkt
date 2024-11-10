#lang typed/racket/base

(require
 (only-in racket/set
          set)
 (only-in racket/match
          define/match))

 ;; xs:qname
;;------------------------------------------------------------

(struct xs:qname
  ([source : (U xs:import xs:schema)]
   [name   : Symbol]))


(provide (struct-out xs:qname))


;; xs:import
;;------------------------------------------------------------

(struct xs:import
  ([prefix              : Symbol]
   [namespace           : String]
   [simple-provide-set  : (Setof Symbol)]
   [complex-provide-set : (Setof Symbol)]))

(provide (struct-out xs:import))


;; xs:schema
;;------------------------------------------------------------

(struct xs:schema
  ([prefix    : Symbol]
   [namespace : String]
   [body      : (Setof xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(define-predicate xs:schema-member?
  xs:schema-member)
       

(provide (struct-out xs:schema)
         xs:schema-member
         xs:schema-member?)


;; xs:element
;;------------------------------------------------------------

(struct xs:element
  ([name       : Symbol]
   [type       : (U xs:qname xs:simple-type xs:complex-type)]
   [min-occurs : Nonnegative-Integer]
   [max-occurs : (U #f Nonnegative-Integer)]))

(provide (struct-out xs:element))


;; xs:simple-type
;;------------------------------------------------------------

(struct xs:simple-type
  ([name        : Symbol]
   [restriction : xs:restriction]))

(provide (struct-out xs:simple-type))


;; xs:restriction
;;------------------------------------------------------------

(struct xs:restriction
  ([base : (U xs:qname xs:simple-type)]
   [body : (Setof xs:restriction-member)]))

(define-type xs:restriction-member
  (U xs:min-inclusive
     xs:min-exclusive
     xs:max-inclusive
     xs:max-exclusive
     xs:enumeration
     xs:pattern
     xs:length
     xs:min-length
     xs:max-length))

(define-predicate xs:restriction-member?
  xs:restriction-member)

(struct xs:min-inclusive
  ([value : Real]))

(struct xs:min-exclusive
  ([value : Real]))

(struct xs:max-inclusive
  ([value : Real]))

(struct xs:max-exclusive
  ([value : Real]))

(struct xs:enumeration
  ([value : String]))

(struct xs:pattern
  ([value : String]))

(struct xs:length
  ([value : Nonnegative-Integer]))

(struct xs:min-length
  ([value : Nonnegative-Integer]))

(struct xs:max-length
  ([value : Nonnegative-Integer]))

(provide (struct-out xs:restriction)
         xs:restriction-member
         xs:restriction-member?
         xs:min-inclusive
         xs:min-exclusive
         xs:max-inclusive
         xs:max-exclusive
         xs:enumeration
         xs:pattern
         xs:length
         xs:min-length
         xs:max-length)


;; xs:complex-type
;;------------------------------------------------------------

(struct xs:complex-type
  ([name          : Symbol]
   [attribute-set : (Setof xs:attribute)]
   [body          : (U #f
                       xs:restriction
                       xs:all
                       xs:choice)]))

(struct xs:all
  ([element-set : (Setof xs:element)]))

(struct xs:choice
  ([min-occurs  : Nonnegative-Integer]
   [max-occurs  : (U #f Nonnegative-Integer)]
   [element-set : (Setof xs:element)]))

(provide (struct-out xs:complex-type)
         (struct-out xs:all)
         (struct-out xs:choice))


;; xs:attribute
;;------------------------------------------------------------

(struct xs:attribute
  ([name     : Symbol]
   [type     : (U xs:qname xs:simple-type)]
   [required : Boolean]))

(provide (struct-out xs:attribute))





