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
  (define-type myStruct1
    ()
    (all
     (id    myStringType     1 1)
     (date  (xs date)        0 unbounded)
     (month myOtherMonthType 0 1)
     ))

  ;; complex type with attributes
  (define-type myStruct2
    ([att1 (xs string)  #f]
     [att2 (xs integer) #t])
    (sequence
     (id    myStringType     1 1)
     (date  (xs date)        0 unbounded)
     (month myOtherMonthType 0 1)
     ))
  )



(define-service demo-service

  (import types demo-schema)

  (define-message error-message)
    

  (define-message op1-input-message
    (body (: types myStruct)))
  
  (define-message op1-output-message
    (body (: types state)))
  

  (define-message op2-input-message)
  (define-message op2-output-message)

  (define-interface firstInterface
    
    (op1 (operation op1-input-message
                    op1-output-message
                    error-message))
    
    (op2 (operation op2-input-message
                    op2-output-message)))

  (define-interface secondInterface))



(display-schema demo-schema)
(display-service demo-service)

