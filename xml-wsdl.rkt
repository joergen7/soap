#lang typed/racket/base

(require
  (only-in racket/match
           match)
  (only-in typed/xml
           XExpr)
  "xml-aux.rkt"
  "xml-schema.rkt")

(provide
 (struct-out wsdl:definitions)
 wsdl:definitions-member
 wsdl:definitions-member?
 (struct-out wsdl:message)
 (struct-out wsdl:part)
 (struct-out wsdl:port-type)
 (struct-out wsdl:operation)
 wsdl->xexpr)


(struct wsdl:definitions
  ([target-namespace : String]
   [import-table     : (HashTable Symbol (U xs:import xs:schema))]
   [body             : (HashTable Symbol wsdl:definitions-member)]))

(define-type wsdl:definitions-member
  (U wsdl:message
     wsdl:port-type))

(define-predicate wsdl:definitions-member?
  wsdl:definitions-member)

(struct wsdl:message
  ([part-table : (HashTable Symbol wsdl:part)]))

(struct wsdl:part
  ([type : (-> qname)]))

(struct wsdl:port-type
  ([operation-table : (HashTable Symbol wsdl:operation)]))

(struct wsdl:operation
  ([input  : (U #f (-> qname))]
   [output : (U #f (-> qname))]
   [fault  : (U #f (-> qname))]))

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

