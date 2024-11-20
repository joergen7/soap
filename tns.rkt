#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 "xs.rkt"
 "wsdl.rkt"
 "ns.rkt"
 "alist.rkt")

;; constants
;;------------------------------------------------------------

(define tns:prefix : Symbol
  'tns)


;; xs:type->string
;;------------------------------------------------------------

(: xs:type->string (-> (U xs:qname xs:simple-type xs:complex-type)
                       String))
(define/match (xs:type->string o)

  ;; xs:qname
  [((xs:qname source ref))
   (xs:qname->string o)]

  ;; xs:simple-type
  [((xs:simple-type name _restriction))
   (format "~a:~a" tns:prefix name)]

  ;; xs:complex-type
  [((xs:complex-type name _attribute-set _body))
   (format "~a:~a" tns:prefix name)])


(provide
 xs:type->string)


;; wsdl:message->string
;;------------------------------------------------------------

(: wsdl:message->string (-> wsdl:message String))
(define/match (wsdl:message->string o)
  [((wsdl:message name _part-set))
   (format "~a:~a" tns:prefix name)])

(provide
 wsdl:message->string)

;; xml:extend-import-table
;;------------------------------------------------------------

(: xml:extend-import-table (-> (Alistof String)
                              String
                              Symbol *
                              (Alistof String)))
(define (xml:extend-import-table import-table tns . sym-list)

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
     (cons
      (cons tns:prefix tns)
      import-table)]

    [(cons hd tl)
     (apply xml:extend-import-table
            (extend import-table hd)
            tns
            tl)]))
     
(provide xml:extend-import-table)






