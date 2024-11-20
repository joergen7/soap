#lang typed/racket/base


(require
 (only-in racket/match
          match)
 (only-in racket/set
          in-set))


(provide
 Alistof
 alist-set
 alist-ref
 alist-has-key?
 alist-union
 alist-apply-union)


(define-type (Alistof t)
  (Listof (Pairof Symbol t)))

;; alist-add
;;------------------------------------------------------------
 
(: alist-set (All (b)
                  (-> (Alistof b)
                      Symbol
                      b
                      (Alistof b))))
(define (alist-set alist x v)
  (if (assq x alist)
      alist
      (cons
       (cons x v)
       alist)))

;; alist-ref
;;------------------------------------------------------------

(: alist-ref (All (b)
                  (-> (Alistof b)
                      Symbol
                      b)))
(define (alist-ref alist x)

  (define pair : (U False (Pairof Symbol b))
    (assq x alist))

  (unless pair
    (raise-argument-error 'alist-ref "valid key" x))

  (cdr pair))


;; alist-has-key?
;;------------------------------------------------------------

(: alist-has-key? (All (b)
                       (-> (Alistof b)
                           Symbol
                           Boolean)))
(define (alist-has-key? alist x)
  (if (assq x alist)
      #t
      #f))


;; alist-union
;;------------------------------------------------------------

(: alist-union (All (b)
                    (-> (Alistof b) *
                        (Alistof b))))
(define (alist-union . alist-list)
  (sort
   (for/fold ([r : (Alistof b)
                 '()])
             ([alist : (Alistof b)
                     (in-list alist-list)])
     (for/fold ([result : (Alistof b)
                        r])
               ([p : (Pairof Symbol b)
                   (in-list alist)])
       (match p
         [(cons k v)
          (alist-set result k v)])))
   (lambda ([x : (Pairof Symbol b)]
            [y : (Pairof Symbol b)])
     (symbol<? (car x) (car y)))))


;; alist-apply-union
;;------------------------------------------------------------

(: alist-apply-union (All (a b)
                          (-> (-> a (Alistof b))
                              (Setof a)
                              (Alistof b))))
(define (alist-apply-union f s)

  (define l : (Listof (Alistof b))
    (for/list ([x : a
                  (in-set s)])
      (f x)))
    
  (apply alist-union l))



