#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          set
          set->list)
 (only-in typed/xml
          XExpr)
 "alist.rkt"
 "xml-element.rkt"
 "wsdl.rkt"
 "ns.rkt"
 "ns-form.rkt"
 "tns.rkt")

(provide
 wsdl:formattable
 wsdl:formattable?
 wsdl:wsdl->xexpr)

(define-type wsdl:formattable
  (U wsdl:definitions
     wsdl:message
     wsdl:part
     wsdl:port-type
     wsdl:operation))

(define-predicate wsdl:formattable?
  wsdl:formattable)

(: wsdl:wsdl->xexpr (-> wsdl:formattable
                        (-> Symbol String)
                        XExpr))
(define (wsdl:wsdl->xexpr o f)

  (match o

  ;; wsdl:definitions
  [(wsdl:definitions name namespace body)

   (define xml-body : (Listof XExpr)
     (for/list ([m : wsdl:definitions-member
                   (in-list
                    (sort (set->list body)
                          wsdl:definitions-member<?))])
       (wsdl:wsdl->xexpr m f)))

   (define import-body : (Listof XExpr)
     (list
      (make-xml-element
       (wsdl import)
       #:att-list (list (cons 'namespace namespace)))))
   
   (make-xml-element
    (wsdl definitions)
    #:prefix-list (xml:extend-import-table
                   (set)
                   namespace
                   wsdl:prefix
                   soap:prefix)
    #:att-list (list
                (cons 'targetNamespace namespace))
    #:body     (append import-body
                       xml-body))]

  ;; wsdl:message
  [(wsdl:message name part-set)

   (define xml-body : (Listof XExpr)
     (for/list ([part : wsdl:part
                      (in-list
                       (sort (set->list part-set)
                             wsdl:part<?))])
       (wsdl:wsdl->xexpr part f)))
     
   (make-xml-element
    (wsdl message)
    #:att-list (list
                (cons 'name (symbol->string name)))
    #:body xml-body)]

  ;; wsdl:part
  [(wsdl:part name type)

   (make-xml-element
    (wsdl part)
    #:name-value name
    #:att-list (list
                (cons 'type (f type))))]

  ;; wsdl:port-type
  [(wsdl:port-type name operation-set)

   (define xml-body : (Listof XExpr)
     (for/list ([x : wsdl:operation
                   (in-list
                    (sort (set->list operation-set)
                          wsdl:operation<?))])
       (wsdl:wsdl->xexpr x f)))
   
   (make-xml-element
    (wsdl portType)
    #:name-value name
    #:body xml-body)]

  ;; wsdl:operation
  [(wsdl:operation name input output fault)

   (: proc (-> Symbol
               (U False Symbol)
               (Listof XExpr)))
   (define (proc key msg)
     (match msg
       [#f
        '()]
       [(? symbol? x)
        (list
         (make-xml-element
          key
          #:att-list (list
                      (cons 'message (f x)))))]))

   (define input-list : (Listof XExpr)
     (proc 'wsdl:input input))

   (define output-list : (Listof XExpr)
     (proc 'wsdl:output output))

   (define fault-list : (Listof XExpr)
     (proc 'wsdl:fault fault))

   (make-xml-element
    (wsdl operation)
    #:name-value name
    #:body (append
            input-list
            output-list
            fault-list))]))



(module+ test

  (require typed/rackunit)

  (: f (-> Symbol String))
  (define (f s)
    (match s
      ['string "xs:string"]
      ['msg    "tns:msg"]))

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:part 'input 'string) f)
   '(wsdl:part ((name "input") (type "xs:string"))))

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:message 'msg (set (wsdl:part 'input 'string))) f)
   '(wsdl:message
     ((name "msg"))
     (wsdl:part ((name "input") (type "xs:string")))))

  (check-equal?
   (wsdl:wsdl->xexpr (wsdl:operation 'op #f #f #f) f)
   '(wsdl:operation
     ((name "op"))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     'msg
     #f
     #f)
    f)
   '(wsdl:operation
     ((name "op"))
     (wsdl:input ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     #f
     'msg
     #f)
    f)
   '(wsdl:operation
     ((name "op"))
     (wsdl:output ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:operation
     'op
     #f
     #f
     'msg)
    f)
   '(wsdl:operation
     ((name "op"))
     (wsdl:fault ((message "tns:msg")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type 'myport (set))
    f)
   '(wsdl:portType ((name "myport"))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type 'myport (set (wsdl:operation 'op #f #f #f)))
    f)
   '(wsdl:portType ((name "myport"))
                   (wsdl:operation ((name "op")))))

  (check-equal?
   (wsdl:wsdl->xexpr
    (wsdl:port-type
     'myport
     (set
      (wsdl:operation 'op1 #f #f #f)
      (wsdl:operation 'op2 #f #f #f)))
    f)
   '(wsdl:portType ((name "myport"))
                   (wsdl:operation ((name "op1")))
                   (wsdl:operation ((name "op2"))))))


           
