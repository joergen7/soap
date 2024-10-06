#lang soap

;; set the namespace to urn:my-example
(in-namespace "urn:my-example")

(define-schema some-schema
  (define-schema-type value (xs string)))

 ;; demo schema
(define-schema demo-schema

  ;; importing other schemas
  ;; -----------------------

  ;; import local schema some-schema
  (import-schema some some-schema)

  ;; import external schema
  (import-schema common "http://company.org/important/common/v4")

  ;; import external schema with list of imported symbols
  (import-schema special "http://company.org/important/special/v1" (yay ok funny))


  ;; defining types
  ;; --------------

  (define-schema-type myStringType (xs string))
)

(display-schema demo-schema)

