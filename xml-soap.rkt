#lang typed/racket/base

(require typed/xml)

(struct s:envelope
  ([header : (Listof s:header-block)]
   [body   : XExpr]))


(struct s:header-block
  ())
