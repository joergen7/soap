#lang typed/racket/base

(struct js:schema
  ([id          : String]
   [name        : Symbol]
   [description : String]
   [type        : js:type])
  #:transparent)

(struct js:property
  ([name     : Symbol]
   [required : Boolean]
   [type     : js:type])
  #:transparent)

(struct js:object
  ([description : String]
   [properties  : (Setof js:property)])
  #:transparent)
  
(struct js:array
  ([description : String])
  #:transparent)

(struct js:string
  ([description : String])
  #:transparent)

(struct js:range
  ([minInclusive : Real]
   [maxInclusive : Real])
  #:transparent)

(struct js:number
  ([description : String]
   [range       : (U #f js:range)])
  #:transparent)

(struct js:boolean
  ([description : String])
  #:transparent)

(struct js:null
  ([description : String])
  #:transparent)
  

(define-type js:type
  (U js:object
     js:array
     js:string
     js:number
     js:boolean
     js:null))
