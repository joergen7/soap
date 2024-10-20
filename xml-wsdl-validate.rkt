#lang typed/racket/base

(require
 (only-in racket/match
          define/match
          match)
 (only-in racket/set
          list->set
          set
          set-add
          set-union)
 "xml-aux.rkt"
 "xml-schema.rkt"
 "xml-wsdl.rkt"
 "xml-static-analysis.rkt")

(provide
 validate-wsdl:definitions)


(: get-message-set (-> wsdl:definitions
                       (Setof Symbol)))
(define/match (get-message-set definitions)
  [((wsdl:definitions _target-namespace _import-table body))
   (for/fold ([result : (Setof Symbol)
                      (set)])
             ([p : (Pairof Symbol wsdl:definitions-member)
                 (in-list (hash->list body))])
     (match p
       
       [(cons name (wsdl:message part-table))
        (set-add result name)]

       [_ result]))])
            
     
(: verify-wsdl:definitions-member (-> (HashTable Symbol (Setof Symbol))
                                      (Setof Symbol)
                                      wsdl:definitions-member
                                      Void))
(define (verify-wsdl:definitions-member provide-table message-set o)

  (define message-table : (HashTable Symbol (Setof Symbol))
    (hash (tns-prefix) message-set))

  (: inspect-part (-> Symbol
                      wsdl:part
                      Void))
  (define (inspect-part x part)
    (with-stack-entry (stack-entry #f x)
      (verify-bind provide-table part)))

  (: inspect-operation (-> Symbol
                           wsdl:operation
                           Void))
  (define (inspect-operation x operation)
    (with-stack-entry (stack-entry #f x)
      (verify-bind message-table operation)))
                             
  (match o

    [(wsdl:message part-table)
     (hash-for-each part-table inspect-part)]

    [(wsdl:port-type operation-table)
     (hash-for-each operation-table inspect-operation)]))

(: verify-wsdl:definitions (-> wsdl:definitions Void))
(define (verify-wsdl:definitions definitions)

  (define provide-table : (HashTable Symbol (Setof Symbol))
    (get-provide-table definitions get-provide-set))

  (define message-set : (Setof Symbol)
    (get-message-set definitions))

  (: proc (-> Symbol
              wsdl:definitions-member
              Void))
  (define (proc x o)
    (match o
      [(wsdl:message _part-table)
       (with-stack-entry (stack-entry 'define-message x)
         (verify-wsdl:definitions-member
          provide-table
          message-set
          o))]
      [(wsdl:port-type _operation-table)
       (with-stack-entry (stack-entry 'define-interface x)
         (verify-wsdl:definitions-member
          provide-table
          message-set
          o))]))

  (match definitions
    [(wsdl:definitions _target-namespace _import-table body)
     (hash-for-each body proc)]))
     

(: validate-wsdl:definitions (-> wsdl:definitions
                                 wsdl:definitions))
(define (validate-wsdl:definitions definitions)
  (verify-wsdl:definitions definitions)
  definitions)
