#lang info
(define collection "soap")
(define deps '("base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-lib" "rackunit-typed"))
(define scribblings '(("scribblings/soap.scrbl" ())))
(define pkg-desc "SOA in Racket")
(define version "0.1.1")
(define pkg-authors '("Jörgen Brandt"))
(define license 'Apache-2.0)
