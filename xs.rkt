#lang typed/racket/base

(require
 (only-in racket/match
          define/match))

;; xs:qname
;;------------------------------------------------------------

(struct xs:qname
  ([prefix : Symbol]
   [name   : Symbol])
  #:transparent)

;; xs:qname
;;------------------------------------------------------------

(: xs:qname->string (-> xs:qname String))
(define/match (xs:qname->string qn)
  [((xs:qname prefix name))
   (format "~a:~a" prefix name)])


(: xs:qname->symbol (-> xs:qname Symbol))
(define (xs:qname->symbol qn)
  (string->symbol (xs:qname->string qn)))

(provide
 (struct-out xs:qname)
 xs:qname->string
 xs:qname->symbol)

;; xs:import
;;------------------------------------------------------------

(struct xs:import
  ([prefix              : Symbol]
   [namespace           : String]
   [simple-provide-set  : (Setof Symbol)]
   [complex-provide-set : (Setof Symbol)])
  #:transparent)

(: xs:import<? (-> xs:import xs:import Boolean))
(define (xs:import<? a b)
  (symbol<? (xs:import-prefix a) (xs:import-prefix b)))
   
  

(provide
 (struct-out xs:import)
 xs:import<?)


;; xs:schema
;;------------------------------------------------------------

(struct xs:schema
  ([name       : Symbol]
   [namespace  : String]
   [import-set : (Setof xs:import)]
   [body       : (Setof xs:schema-member)]))

(define-type xs:schema-member
  (U xs:element
     xs:simple-type
     xs:complex-type))

(define-predicate xs:schema-member?
  xs:schema-member)

(: xs:schema-member<? (-> xs:schema-member xs:schema-member Boolean))
(define (xs:schema-member<? a b)

  (: name (-> xs:schema-member Symbol))
  (define/match (name x)
    [((xs:element name _type _min-occurs _max-occurs))
     name]
    [((xs:simple-type name _restriction))
     name]
    [((xs:complex-type name _attribute-set _body))
     name])

  (symbol<? (name a) (name b)))
     
       

(provide (struct-out xs:schema)
         xs:schema-member
         xs:schema-member?
         xs:schema-member<?)


;; xs:element
;;------------------------------------------------------------

(struct xs:element
  ([name       : Symbol]
   [type       : Symbol] ; (U xs:qname xs:simple-type xs:complex-type)
   [min-occurs : Nonnegative-Integer]
   [max-occurs : (U False Nonnegative-Integer)])
  #:transparent)

(: xs:element<? (-> xs:element xs:element Boolean))
(define (xs:element<? a b)
  (symbol<? (xs:element-name a) (xs:element-name b)))

(provide
 (struct-out xs:element)
 xs:element<?)


;; xs:simple-type
;;------------------------------------------------------------

(struct xs:simple-type
  ([name        : Symbol]
   [restriction : xs:restriction])
  #:transparent)

(provide (struct-out xs:simple-type))


;; xs:restriction
;;------------------------------------------------------------

(struct xs:restriction
  ([base : Symbol] ; (U xs:qname xs:simple-type)
   [body : (Setof xs:restriction-member)])
  #:transparent)

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
  ([value : Real])
  #:transparent)

(struct xs:min-exclusive
  ([value : Real])
  #:transparent)

(struct xs:max-inclusive
  ([value : Real])
  #:transparent)

(struct xs:max-exclusive
  ([value : Real])
  #:transparent)

(struct xs:enumeration
  ([value : String])
  #:transparent)

(struct xs:pattern
  ([value : String])
  #:transparent)

(struct xs:length
  ([value : Nonnegative-Integer])
  #:transparent)

(struct xs:min-length
  ([value : Nonnegative-Integer])
  #:transparent)

(struct xs:max-length
  ([value : Nonnegative-Integer])
  #:transparent)

(provide
 (struct-out xs:restriction)
 xs:restriction-member
 xs:restriction-member?
 (struct-out xs:min-inclusive)
 (struct-out xs:min-exclusive)
 (struct-out xs:max-inclusive)
 (struct-out xs:max-exclusive)
 (struct-out xs:enumeration)
 (struct-out xs:pattern)
 (struct-out xs:length)
 (struct-out xs:min-length)
 (struct-out xs:max-length))


;; xs:complex-type
;;------------------------------------------------------------

(struct xs:complex-type
  ([name          : Symbol]
   [attribute-set : (Setof xs:attribute)]
   [body          : (U #f
                       xs:restriction
                       xs:all
                       xs:choice)])
  #:transparent)

(struct xs:all
  ([element-set : (Setof xs:element)])
  #:transparent)

(struct xs:choice
  ([min-occurs  : Nonnegative-Integer]
   [max-occurs  : (U #f Nonnegative-Integer)]
   [element-set : (Setof xs:element)])
  #:transparent)

(provide (struct-out xs:complex-type)
         (struct-out xs:all)
         (struct-out xs:choice))


;; xs:attribute
;;------------------------------------------------------------

(struct xs:attribute
  ([name     : Symbol]
   [type     : Symbol] ; (U xs:qname xs:simple-type)
   [required : Boolean])
  #:transparent)

(: xs:attribute<? (-> xs:attribute xs:attribute Boolean))
(define (xs:attribute<? a b)
  (symbol<? (xs:attribute-name a) (xs:attribute-name b)))

(provide
 (struct-out xs:attribute)
 xs:attribute<?)          


(module+ test

  (require
   (only-in typed/rackunit
            check-equal?))

  (check-equal? (xs:qname->string
                 (xs:qname
                  'bla
                  'blub))
                "bla:blub"))






