#lang racket/base

(require
 (only-in xml
          p-i))

(provide xml-p-i)

(define xml-p-i
  (p-i
   #f
   #f
   'xml
   "version=\"1.0\" encoding=\"UTF-8\""))

