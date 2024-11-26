#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          in-set)
 "xs.rkt"
 "wsdl.rkt"
 "ns.rkt"
 "ns-form.rkt"
 "alist.rkt")

;; constants
;;------------------------------------------------------------

(define tns:prefix : Symbol
  'tns)





;; xml:type->string
;;------------------------------------------------------------

(: xml:type->string (-> (U xs:qname xs:simple-type xs:complex-type wsdl:message)
                        String))
(define/match (xml:type->string o)

  ;; xs:qname
  [((xs:qname source ref))
   (xs:qname->string o)]

  ;; xs:simple-type
  [((xs:simple-type name _restriction))
   (format "~a:~a" tns:prefix name)]

  ;; xs:complex-type
  [((xs:complex-type name _attribute-set _body))
   (format "~a:~a" tns:prefix name)]

  ;; wsdl:message
  [((wsdl:message name _part-set))
   (format "~a:~a" tns:prefix name)])


(provide
 xml:type->string)



;; xml:extend-import-table
;;------------------------------------------------------------


(define xml:prefix-table : (Immutable-HashTable Symbol String)
  (hash xs:prefix   "http://www.w3.org/2001/XMLSchema"
        wsdl:prefix "http://schemas.xmlsoap.org/wsdl"
        soap:prefix "http://schemas.xmlsoap.org/wsdl/soap"))

(: xml:extend-import-table (-> (Setof xs:import)
                               String
                               Symbol *
                               (Alistof String)))
(define (xml:extend-import-table import-set tns . sym-list)

  (: extend (-> (Alistof String)
                Symbol
                (Alistof String)))
  (define (extend t s)
    (if (alist-has-key? t s)
        t
        (let ([pair : (Pairof Symbol String)
                    (cons
                     s
                     (hash-ref
                      xml:prefix-table
                      s
                      (lambda ()
                        (raise-user-error 'xml:extend-import-table "namespace prefix undefined: " s))))])
          (cons pair t))))


  (match sym-list
    ['()

     (define import-table : (Alistof String)
       (for/list ([x : xs:import
                     (in-set import-set)])
         (match x
           [(xs:import prefix namespace _simple-provide-set _complex-provide-set)
            (cons prefix namespace)])))

     (cons
      (cons tns:prefix tns)
      import-table)]

    [(cons hd tl)
     (extend
      (apply xml:extend-import-table
             import-set tns tl)
      hd)]))

(provide xml:extend-import-table)



(module+ test

  (require
    (only-in typed/rackunit
             check-equal?)
    (only-in racket/set
             set))


  (define a-restriction : xs:restriction
    (xs:restriction
     'string
     (set)))  

  (check-equal? (xml:type->string
                 (xs string))
                "xs:string")

  (check-equal? (xml:type->string
                 (xs:simple-type
                  'blub
                  a-restriction))
                "tns:blub")

  (check-equal? (xml:type->string
                 (xs:complex-type
                  'bla
                  (set)
                  #f))
                "tns:bla"))


