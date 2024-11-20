#lang soap

;; set the namespace to urn:my-example
(in-namespace "urn:my-example")


;; some-schema
;;------------------------------------------------------------

(define-type valueType
  (as string))

(define-schema some-schema
  valueType)


;; demo-schema
;;------------------------------------------------------------

(import common "http://company.org/important/common/v4" () ())
(import special "http://company.org/important/special/v1" (yay ok funny) (foo bar bla blub))

;; plain string type
(define-type myStringType
  (as string))

;; regex
(define-type myMonthType
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

;; complex type: element only
(define-type myStruct1
  ()
  (all
   (id    myStringType     1 1)
   (oDate date             0 unbounded)
   (month myOtherMonthType 0 1)))

;; complex type: attribute only
(define-type myStruct2
  ([att1 string  optional]
   [att2 integer required]))
    

;; complex type: attributes and elements
(define-type myStruct3
  ([att1 string  optional]
   [att2 integer required])
  (all
   [id    myStringType     1 1]
   [oDate date             0 unbounded]
   [month myOtherMonthType 0 1]))

;; complex type: simple content
(define-type myStruct4
  ([att1 string  optional]
   [att2 integer required])
  (as myStringType))


(define-schema demo-schema
  myMonthType
  score
  state
  myStruct1
  myStruct2
  myStruct3
  myStruct4)

(display-schema demo-schema)




(define-message error-message)

(define-message op1-input-message
  [body myStruct1])
  
(define-message op1-output-message
  [body state])
  
(define-message op2-input-message)

(define-message op2-output-message)


(define-interface firstInterface
    
  (with-input op1-input-message
    (with-output op1-output-message
      (with-fault error-message
        (op op1))))

  (with-input op2-input-message
    (with-output op2-output-message
      (op op2))))

(define-interface secondInterface)




(define-service demo-service
  firstInterface
  secondInterface)

(display-service demo-service)


#|
(define-service demo-service

  (import types demo-schema)

  (define-message error-message)
    

  (define-message op1-input-message
    [body (: types myStruct1)])
  
  (define-message op1-output-message
    (body (: types state)))
  

  (define-message op2-input-message)
  (define-message op2-output-message)

  (define-interface firstInterface
    
    [op1 (input  op1-input-message)
         (output op1-output-message)
         (fault  error-message)]
    
    [op2 (input  op2-input-message)
         (output op2-output-message)])

  (define-interface secondInterface))




(display-service demo-service)
|#

