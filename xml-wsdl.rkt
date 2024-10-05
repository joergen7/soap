#lang typed/racket/base

(require
  (only-in racket/match
           define/match)
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
   [import-list      : (Listof (Pairof Symbol (U xs:import xs:schema)))]
   [body             : (Listof wsdl:definitions-member)]))

(define-type wsdl:definitions-member
  (U wsdl:message wsdl:port-type))

(define-predicate wsdl:definitions-member?
  wsdl:definitions-member)

(struct wsdl:message
  ([name      : Symbol]
   [part-list : (Listof wsdl:part)]))

(struct wsdl:part
  ([name : Symbol]
   [type : (-> qname)]))

(struct wsdl:port-type
  ([name : Symbol]
   [operation-list : (Listof wsdl:operation)]))

(struct wsdl:operation
  ([name   : Symbol]
   [input  : (U #f (-> qname))]
   [output : (U #f (-> qname))]
   [fault  : (U #f (-> qname))]))

(: wsdl->xexpr (-> Any XExpr))
(define/match (wsdl->xexpr x)

  [((wsdl:definitions target-namespace import-list body))
   (let ([a-prefix-list (get-import-attribute-list import-list)]
         [b-prefix-list (list (cons (wsdl-prefix) "http://schemas.xmlsoap.org/wsdl/")
                              (cons (tns-prefix)  target-namespace))])
     (make-xml-element
      ((wsdl definitions))
      #:prefix-list (append a-prefix-list b-prefix-list)
      (list (cons 'targetNamespace target-namespace))
      (append (get-import-xexpr-list import-list ((wsdl import)))
              (map wsdl->xexpr body))))]

  [((wsdl:message name part-list))
   (make-xml-element
    ((wsdl message))
    (list (cons 'name (symbol->string name)))
    (map wsdl->xexpr part-list))]

  [((wsdl:part name type))
   (make-xml-element
    ((wsdl part))
    (list (cons 'name (symbol->string name))
          (cons 'type (qname->string (type)))))]

  [((wsdl:port-type name operation-list))
   (make-xml-element
    ((wsdl portType))
    (list (cons 'name (symbol->string name)))
    (map wsdl->xexpr operation-list))]

  [((wsdl:operation name input output fault))
   (let ([input-list : (Listof XExpr)
                     (if input
                         (list (make-xml-element ((wsdl input)) (list (cons 'message (qname->string (input))))))
                         '())]
         [output-list : (Listof XExpr)
                      (if output
                          (list (make-xml-element ((wsdl output)) (list (cons 'message (qname->string (output))))))
                          '())]
         [fault-list : (Listof XExpr)
                     (if fault
                         (list (make-xml-element ((wsdl fault)) (list (cons 'message (qname->string (fault))))))
                         '())])

     (make-xml-element
      ((wsdl operation))
      (list (cons 'name (symbol->string name)))
      (append
       input-list
       output-list
       fault-list)))])

