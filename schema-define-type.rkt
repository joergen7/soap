#lang typed/racket/base

(require
 (for-syntax
  (only-in racket/base
           #%app
           #%top
           #%datum
           with-syntax
           quote
           syntax)
  (only-in racket/syntax
           format-id)
  (only-in syntax/parse
           syntax-parse
           id
           exact-integer
           number
           str))
 (only-in racket/set
          set)
 "schema-lang.rkt")

(provide schema-define-type)



(define-syntax (schema-define-type stx)
  (syntax-parse stx
    #:datum-literals
    (boolean string natural integer real date record list enum range-int range-real pattern union :)

    [(_ name:id boolean)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name Boolean)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''boolean)))]

    [(_ name:id string)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name String)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''string)))]

    [(_ name:id natural)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name Natural)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''natural)))]

    [(_ name:id integer)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name Integer)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''integer)))]

    [(_ name:id real)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name Real)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''real)))]

    [(_ name:id date)
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name date)
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #''date)))]

    [(_ name:id (record ([f_i : t_i] ...) ([f_j : t_j] ...)))
     (with-syntax ([name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (struct name
             ([f_i : t_i] ... [f_j : (U False t_j)] ...) ; TODO: handle optional Booleans
             #:transparent)
           (define-syntax (name-schema stx)
             #'(t-record
                'name
                (make-immutable-hash '((f_i . t_i) ...))
                (make-immutable-hash '((f_j . t_j) ...))))))]

    [(_ name:id (list t:id))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name (Listof t))
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #'(t-list 'name 't))))]

    [(_ name:id (enum x_i:id ...))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
     #'(begin
         (define-type name (U 'x_i ...))
         (define-predicate name? name)
         (define-syntax (name-schema stx)
           #'(t-enum 'name (set 'x_i ...)))))]

    [(_ name:id (range-int lo:exact-integer hi:exact-integer))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
     #'(begin
         (define-type name Integer)
         (: name? (-> Any Boolean))
         (define (name? x) (and (integer? x) (>= x lo) (<= x hi)))
         (define-syntax (name-schema stx) #'(t-range-int 'name lo hi))))]

    [(_ name:id (range-real lo:number hi:number))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
     #'(begin
         (define-type name Real)
         (: name? (-> Any Boolean))
         (define (name? x) (and (real? x) (>= x lo) (<= x hi)))
         (define-syntax (name-schema stx) #'(t-range-real 'name lo hi))))]

    [(_ name:id (pattern s:str))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
     #'(begin
         (define-type name String)
         (: name? (-> Any Boolean))
         (define (name? x) (if (and (string? x) (regexp-match s x)) #t #f))
         (define-syntax (name-schema stx) #'(t-pattern 'name (regexp s)))))]

    [(_ name:id (union t_i ...))
     (with-syntax ([name?       (format-id #'name "~a?" #'name)]
                   [name-schema (format-id #'name "~a-schema" #'name)])
       #'(begin
           (define-type name (U t_i ...))
           (define-predicate name? name)
           (define-syntax (name-schema stx)
             #'(t-union 'name (set 't_i ...)))))]))

