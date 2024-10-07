#lang soap

;; set the namespace to urn:my-example
(in-namespace "urn:my-example")

(define-schema some-schema
  (define-type value (xs string)))

 ;; demo schema
(define-schema demo-schema

  ;; importing other schemas
  ;; -----------------------

  ;; import local schema some-schema
  (import some some-schema)

  ;; import external schema
  (import common "http://company.org/important/common/v4")

  ;; import external schema with list of imported symbols
  (import special "http://company.org/important/special/v1" (yay ok funny))


  ;; defining types
  ;; --------------

  ;; plain string type
  (define-type myStringType
    (xs string))

  ;; regex
  (define-type myMonthType
    (xs string)
    (pattern "(0[1-9])|(1[0-2])"))

  ;; integer range
  (define-type myOtherMonthType
    (range 1 12))

  ;; decimal range
  (define-type score
    (range 1.0 10.0))

  ;; enumeration
  (define-type state
    (enum "ok" "faulty" "foobar"))

  ;; complex type without attributes
  (define-type myStruct
    ()
    (all
     (id    (element (tns myStringType) 1 1))
     (date  (element (xs date) 0 1))
     (month (element (tns myOtherMonthType) 0 1))))
)

(display-schema demo-schema)

