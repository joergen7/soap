#lang soap

(in-namespace "urn:example1")

(define-type countryCodeType
  (pattern "\\d{2}"))

(define-type areaType
  (pattern "\\d{3}"))

(define-type exchangeType
  (pattern "\\d{3}"))

(define-type numberType
  (pattern"\\d{4}"))

(define-type telephoneNumberType
  ()
  (all
   (countryCode countryCodeType 1 1)
   (area        areaType        1 1)
   (exchange    exchangeType    1 1)
   (number      numberType      1 1)))

(display-schema schema1)
