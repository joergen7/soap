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
 "alist.rkt"
 "xml-element.rkt"
 "wsdl.rkt"
 "ns.rkt"
 "ns-forms.rkt"
 "tns.rkt"
 "import-table.rkt"
 "wsdl-member-table.rkt")

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
                        XExpr))
(define/match (wsdl:wsdl->xexpr o)

  ;; wsdl:definitions
  [((wsdl:definitions name namespace body))

   (define member-table : (Alistof wsdl:definitions-member)
     (alist-apply-union
      wsdl:collect-member-table
      body))

   (define xml-body : (Listof XExpr)
     (for/list ([p : (Pairof Symbol wsdl:definitions-member)
                   (in-list member-table)])
       (match p
         [(cons _name m)
          (wsdl:wsdl->xexpr m)])))
   
   (define import-table : (Alistof String)
     (xml:collect-import-table o))

   (define import-body : (Listof XExpr)
     (for/fold ([result : (Listof XExpr)
                        '()])
               ([pair : (Pairof Symbol String)
                      (in-list (xml:extend-import-table import-table namespace))])
       (match pair
         [(cons prf ns)
          (if (hash-has-key? xml:prefix-table prf)
              result
              (cons
               (make-xml-element
                (wsdl import)
                #:att-list (list
                            (cons 'namespace ns)))
               result))])))

   (make-xml-element
    (wsdl definitions)
    #:prefix-list (xml:extend-import-table
                   import-table
                   namespace
                   wsdl:prefix
                   soap:prefix)
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
    (wsdl message)
    #:att-list (list
                (cons 'name (symbol->string name)))
    #:body xml-body)]

  ;; wsdl:part
  [((wsdl:part name type))

   (make-xml-element
    (wsdl part)
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
    (wsdl portType)
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
                      (cons 'message (wsdl:message->string msg)))))]))

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
                   (wsdl:operation ((name "op2"))))))


           
