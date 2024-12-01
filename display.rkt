#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           syntax)
  (only-in syntax/parse
           syntax-parse
           str))
 (only-in typed/xml
          empty-tag-shorthand
          display-xml
          document
          prolog
          xexpr->xml
          element?
          P-I
          XExpr)
 "ns-form.rkt"
 "xs.rkt"
 "xs-format.rkt"
 "wsdl.rkt"
 "wsdl-format.rkt")

(require/typed
    "display-aux.rkt"
  [xml-p-i P-I])


(provide
 with-output-file
 display-schema
 display-service)

(define xml:shorthand-list : (Listof Symbol)
  (map
   xs:qname->symbol
   (list
    (xs import)
    (xs element)
    (xs minInclusive)
    (xs minExclusive)
    (xs maxInclusive)
    (xs maxExclusive)
    (xs enumeration)
    (xs pattern)
    (xs length)
    (xs minLength)
    (xs maxLength)
    (xs attribute)
    (wsdl import)
    (wsdl input)
    (wsdl output)
    (wsdl fault)
    (wsdl part))))

(: display-xexpr (-> XExpr Void))
(define (display-xexpr e)
  (parameterize ([empty-tag-shorthand xml:shorthand-list])
    (display-xml
     (document (prolog
                (list xml-p-i)
                #f
                '())
               (assert (xexpr->xml e) element?)
               '()))))

(: display-schema (-> xs:schema
                      (-> Symbol String)
                      Void))
(define (display-schema e f)
  (display-xexpr (xs:xs->xexpr e f)))

(: display-service (-> wsdl:definitions
                       (-> Symbol String)
                       Void))
(define (display-service e f)
  (display-xexpr (wsdl:wsdl->xexpr e f)))

(define-syntax (with-output-file stx)
  (syntax-parse stx
    [(_ path:str e ...)
     #'(call-with-output-file
         path
         (lambda ([out : Output-Port])
           (parameterize ([current-output-port out])
             e ...))
         #:mode   'text
         #:exists 'replace)]))


