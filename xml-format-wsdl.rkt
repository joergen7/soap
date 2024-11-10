#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          in-set
          set)
 (only-in typed/xml
          XExpr)
 "xml-element.rkt"
 "xml-wsdl.rkt"
 "xml-format.rkt")

#|
(: wsdl->xexpr (->* (Any) (#:name-value (U #f Symbol)) XExpr))
(define (wsdl->xexpr x #:name-value (name-value #f))


  (: proc (-> Symbol Any XExpr))
  (define (proc name elem)
    (wsdl->xexpr elem #:name-value name))

  (match x

    ;; wsdl:definitions
    [(wsdl:definitions target-namespace import-table body)
     (let ([a-prefix-list : (Listof (Pairof Symbol String))
                          (get-import-attribute-list import-table)]
           [b-prefix-list : (Listof (Pairof Symbol String))
                          (list (cons (wsdl-prefix) "http://schemas.xmlsoap.org/wsdl/")
                                (cons (tns-prefix)  target-namespace))]
           [a-body : (Listof XExpr)
                   (get-import-xexpr-list import-table ((wsdl import)))]
           [b-body : (Listof XExpr)
                   (hash-map body proc)])
       (make-xml-element
        ((wsdl definitions))
        #:prefix-list (append a-prefix-list b-prefix-list)
        #:att-list    (list (cons 'targetNamespace target-namespace))
        #:body        (append a-body b-body)))] 

    ;; wsdl:message
    [(wsdl:message part-table)
     (make-xml-element
      ((wsdl message))
      #:name-value name-value
      #:body       (hash-map part-table proc))]

    ;; wsdl:part
    [(wsdl:part type)
     (make-xml-element
      ((wsdl part))
      #:name-value name-value
      #:att-list (list (cons 'type (qname->string (type)))))]

    ;; wsdl:port-type
    [(wsdl:port-type operation-table)
     (make-xml-element
      ((wsdl portType))
      #:name-value name-value
      #:body       (hash-map operation-table proc))]

    ;; wsdl:operation
    [(wsdl:operation input output fault)
     (let ([input-list : (Listof XExpr)
                       (if input
                           (list (make-xml-element ((wsdl input)) #:att-list (list (cons 'message (qname->string (input))))))
                           '())]
           [output-list : (Listof XExpr)
                        (if output
                            (list (make-xml-element ((wsdl output)) #:att-list (list (cons 'message (qname->string (output))))))
                            '())]
           [fault-list : (Listof XExpr)
                       (if fault
                           (list (make-xml-element ((wsdl fault)) #:att-list (list (cons 'message (qname->string (fault))))))
                           '())])

       (make-xml-element
        ((wsdl operation))
        #:name-value name-value
        #:body       (append
                      input-list
                      output-list
                      fault-list)))]))

|#

(define-type wsdl:formattable
  (U wsdl:definitions
     wsdl:message
     wsdl:part
     wsdl:port-type
     wsdl:operation))

(: wsdl:wsdl->xexpr (-> wsdl:formattable
                        XExpr))
(define/match (wsdl:wsdl->xexpr o)

  ;; wsdl:definitions
  [((wsdl:definitions namespace body))

   (define import-table : (Listof (Pairof Symbol String))
     (xs:collect-import-table o))

   (define import-body : (Listof XExpr)
     (for/fold ([result : (Listof XExpr)
                        '()])
               ([pair : (Pairof Symbol String)
                      (in-list import-table)])
       (match pair
         [(cons prf ns)
          (if (hash-has-key? xml:prefix-table prf)
              result
              (cons
               (make-xml-element
                'wsdl:import
                #:att-list (list
                            (cons 'namespace ns)))
               result))])))

   (define xml-body : (Listof XExpr)
     (for/list ([x : wsdl:definitions-member
                   (in-set body)])
       (wsdl:wsdl->xexpr x)))
   
   (make-xml-element
    'wsdl:definitions
    #:prefix-list (xs:extend-import-table
                   import-table
                   namespace
                   xml:prefix-wsdl
                   xml:prefix-soap)
    #:att-list (list
                (cons 'targetNamespace namespace))
    #:body     (append import-body
                       xml-body))]

  ;; wsdl:message
  [((wsdl:message name part-set))

   (define xml-body : (Listof XExpr)
     (for/list ([part : wsdl:part
                      (in-set part-set)])
       (wsdl:wsdl->xexpr part)))
     
   (make-xml-element
    'wsdl:message
    #:att-list (list
                (cons 'name (symbol->string name)))
    #:body xml-body)]

  ;; wsdl:part
  [((wsdl:part name type))

   (make-xml-element
    'wsdl:part
    #:name-value name
    #:att-list (list
                (cons 'type (xs:type->string type))))]

  ;; wsdl:port-type
  [((wsdl:port-type name operation-set))

   (define xml-body : (Listof XExpr)
     (for/list ([x : wsdl:operation
                   (in-set operation-set)])
       (wsdl:wsdl->xexpr x)))
   
   (make-xml-element
    'wsdl:portType
    #:name-value name
    #:body xml-body)]

  ;; wsdl:operation
  [((wsdl:operation name input output fault))

   (: proc (-> Symbol
               (U False wsdl:message)
               (Listof XExpr)))
   (define (proc key msg)
     (match msg
       [#f
        '()]
       [(wsdl:message name _part-set)
        (list
         (make-xml-element
          key
          #:att-list (list
                      (cons 'message (xs:type->string msg)))))]))

   (define input-list : (Listof XExpr)
     (proc 'wsdl:input input))

   (define output-list : (Listof XExpr)
     (proc 'wsdl:output output))

   (define fault-list : (Listof XExpr)
     (proc 'wsdl:fault fault))

   (make-xml-element
    'wsdl:operation
    #:name-value name
    #:body (append
            input-list
            output-list
            fault-list))])



(module+ test

  (require typed/rackunit)

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:part 'input (xs string)))
   '(wsdl:part ((name "input") (type "xs:string"))))

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:message 'msg (set (wsdl:part 'input (xs string)))))
   '(wsdl:message
     ((name "msg"))
     (wsdl:part ((name "input") (type "xs:string")))))

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:operation 'op #f #f #f))
   '(wsdl:operation
     ((name "op"))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     (wsdl:message 'msg (set (wsdl:part 'input (xs string))))
     #f
     #f))
   '(wsdl:operation
     ((name "op"))
     (wsdl:input ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     #f
     (wsdl:message 'msg (set (wsdl:part 'output (xs string))))
     #f))
   '(wsdl:operation
     ((name "op"))
     (wsdl:output ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     #f
     #f
     (wsdl:message 'msg (set (wsdl:part 'fault (xs string))))))
   '(wsdl:operation
     ((name "op"))
     (wsdl:fault ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type 'myport (set)))
   '(wsdl:portType ((name "myport"))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type 'myport (set (wsdl:operation 'op #f #f #f))))
   '(wsdl:portType ((name "myport"))
                   (wsdl:operation ((name "op")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type
     'myport
     (set
      (wsdl:operation 'op1 #f #f #f)
      (wsdl:operation 'op2 #f #f #f))))
   '(wsdl:portType ((name "myport"))
                   (wsdl:operation ((name "op1")))
                   (wsdl:operation ((name "op2")))))


           
