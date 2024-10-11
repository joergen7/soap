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
                                      Symbol
                                      wsdl:definitions-member
                                      Void))
(define (verify-wsdl:definitions-member provide-table message-set name o)

  (define message-table : (HashTable Symbol (Setof Symbol))
    (hash (tns-prefix) message-set))

  (: inspect-part (-> Symbol
                      wsdl:part
                      Void))
  (define (inspect-part x part)
    
    (define loc : String
      (format "message ~a/part ~a" name x))

    (verify-bind provide-table loc part))

  (: inspect-operation (-> Symbol
                           wsdl:operation
                           Void))
  (define (inspect-operation x operation)

    (define loc : String
      (format "interface ~a/operation ~a" name x))

    (verify-bind message-table loc operation))
                             
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

  (: ver (-> Symbol
             wsdl:definitions-member
             Void))
  (define (ver x o)
    (verify-wsdl:definitions-member
     provide-table
     message-set
     x
     o))

  (match definitions
    [(wsdl:definitions _target-namespace _import-table body)
     (hash-for-each body ver)]))
     

(: validate-wsdl:definitions (-> wsdl:definitions
                                 wsdl:definitions))
(define (validate-wsdl:definitions definitions)
  (verify-wsdl:definitions definitions)
  definitions)
