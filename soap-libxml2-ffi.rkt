#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/match

         (for-syntax (only-in syntax/parse syntax-parse)
                     (only-in racket/base syntax)))


;;==========================================================
;; Error Handling
;;==========================================================

(struct exn:fail:xml2 exn:fail      () #:transparent)

(define-syntax (raise-xml2-error stx)
  (syntax-parse stx
    [(_ who reason)
     #'(raise
        (exn:fail:xml2
         (format "~a: ~a" who reason)
         (current-continuation-marks)))]))

(define (check-not-null result who reason)
  (or result
      (raise-xml2-error who reason)))

(define/match (check-valid result msg)
  [(0   _)   (void)]
  [(1  msg) (raise-xml2-error "xmlSchemaValidateDoc" (string-append "no root\n" msg))]
  [(2  msg) (raise-xml2-error "xmlSchemaValidateDoc" (string-append "undeclared elem\n" msg))]
  ;; ...
  [((? exact-integer? n) msg) (raise-xml2-error "xmlSchemaValidateDoc" (format "~a\n~a" n msg))])


;;==========================================================
;; Data Validation
;;==========================================================

(define (validate-bytes x name)
  (cond [(bytes? x)  x]
        [(string? x) (string->bytes/utf-8 x)]
        [#t          (error (format "~a must be either bytes or string" name))]))


;;==========================================================
;; Constants
;;==========================================================

(define XML_PARSE_RECOVER 1) ; recover on errors
(define XML_PARSE_NOENT   2) ; substitute entities
(define XML_PARSE_DTDLOAD 4) ; load the external subset
(define XML_PARSE_DTDATTR 8) ; default DTD attributes
#|
    XML_PARSE_DTDVALID = 16 /* validate with the DTD */
    XML_PARSE_NOERROR = 32 /* suppress error reports */
    XML_PARSE_NOWARNING = 64 /* suppress warning reports */
    XML_PARSE_PEDANTIC = 128 /* pedantic error reporting */
    XML_PARSE_NOBLANKS = 256 /* remove blank nodes */
    XML_PARSE_SAX1 = 512 /* use the SAX1 interface internally */
    XML_PARSE_XINCLUDE = 1024 /* Implement XInclude substitution */
    XML_PARSE_NONET = 2048 /* Forbid network access */
    XML_PARSE_NODICT = 4096 /* Do not reuse the context dictionary */
    XML_PARSE_NSCLEAN = 8192 /* remove redundant namespaces declarations */
    XML_PARSE_NOCDATA = 16384 /* merge CDATA as text nodes */
    XML_PARSE_NOXINCNODE = 32768 /* do not generate XINCLUDE START/END nodes */
    XML_PARSE_COMPACT = 65536 /* compact small text nodes; no modification of the tree allowed afterwards (will possibly crash if you try to modify the tree) */
    XML_PARSE_OLD10 = 131072 /* parse using XML-1.0 before update 5 */
    XML_PARSE_NOBASEFIX = 262144 /* do not fixup XINCLUDE xml:base uris */
    XML_PARSE_HUGE = 524288 /* relax any hardcoded limit from the parser */
    XML_PARSE_OLDSAX = 1048576 /* parse using SAX2 interface before 2.7.0 */
    XML_PARSE_IGNORE_ENC = 2097152 /* ignore internal document encoding hint */
    XML_PARSE_BIG_LINES = 4194304 /*  Store big lines numbers in text PSVI field */
|#

;;==========================================================
;; Data Types
;;==========================================================

(define _xmlDoc-pointer              (_cpointer 'xmlDoc))
(define _xmlParserCtxt-pointer       (_cpointer 'xmlParserCtxt))
(define _xmlSchemaParserCtxt-pointer (_cpointer 'xmlSchemaParserCtxt))
(define _xmlSchema-pointer           (_cpointer 'xmlSchema))
(define _xmlSchemaValidCtxt-pointer  (_cpointer 'xmlSchemaValidCtxt))
(define _xmlNode-pointer             (_cpointer 'xmlNode))

(define _xmlErrorLevel
  (_enum '(none
           warning
           error
           fatal)))

(define-cstruct _xmlError
  ([domain  _int]
   [code    _int]
   [message _string/utf-8]
   [level   _xmlErrorLevel]
   [file    _string/utf-8]
   [line    _int]
   [str1    _string/utf-8]
   [str2    _string/utf-8]
   [str3    _string/utf-8]
   [int1    _int]
   [int2    _int]
   [ctxt    (_or-null _xmlParserCtxt-pointer)]
   [node    (_or-null _xmlNode-pointer)]))
  


;;==========================================================
;; FFI
;;==========================================================

(define-ffi-definer define-libxml2
  (ffi-lib "libxml2" '("2.9.13" "2" #f)))

;; xmlFreeParserCtxt
(define-libxml2 xmlFreeParserCtxt
  (_fun
   _xmlParserCtxt-pointer
   -> _void)
  #:wrap (deallocator))

;; xmlNewParserCtxt
(define-libxml2 xmlNewParserCtxt
  (_fun
   -> (result : _xmlParserCtxt-pointer)
   -> (check-not-null result "xmlNewParserCtxt" "cannot create parser context"))
  #:wrap (allocator xmlFreeParserCtxt))

;; xmlCtxtGetLastError
(define-libxml2 xmlCtxtGetLastError
  (_fun
   _xmlParserCtxt-pointer
   -> (_or-null _xmlError-pointer)))


;; xmlFreeDoc
(define-libxml2 xmlFreeDoc
  (_fun
   _xmlDoc-pointer
   -> _void)
  #:wrap (deallocator))

(define-libxml2 xmlCtxtReadDoc
  (_fun
   _xmlParserCtxt-pointer ; ctxt
   _bytes/nul-terminated  ; str
   _string/utf-8          ; URL
   _string/utf-8          ; encoding
   _int                   ; options
   -> (result : _xmlDoc-pointer)
   -> (check-not-null result "xmlCtxtReadDoc" "cannot read document"))
  #:wrap (allocator xmlFreeDoc))

(define-libxml2 xmlReadDoc
  (_fun
   _bytes/nul-terminated  ; str
   _string/utf-8          ; URL
   _string/utf-8          ; encoding
   _int                   ; options
   -> (result : _xmlDoc-pointer)
   -> (check-not-null result "xmlReadDoc" "cannot read document"))
  #:wrap (allocator xmlFreeDoc))

(define-libxml2 xmlReadMemory
  (_fun
   _bytes                 ; buffer
   _int                   ; size
   _string/utf-8          ; URL
   _string/utf-8          ; encoding
   _int                   ; options
   -> (result : _xmlDoc-pointer)
   -> (check-not-null result "xmlReadMemory" "cannot read document"))
  #:wrap (allocator xmlFreeDoc))

;; xmlSchemaFreeParserCtxt
(define-libxml2 xmlSchemaFreeParserCtxt
  (_fun
   _xmlSchemaParserCtxt-pointer
   -> _void)
  #:wrap (deallocator))

;; xmlSchemaNewDocParserCtxt
(define-libxml2 xmlSchemaNewDocParserCtxt
  (_fun
   _xmlDoc-pointer
   -> (result : _xmlSchemaParserCtxt-pointer)
   -> (check-not-null result "xmlSchemaNewDocParserCtxt" "cannot create schema parser context"))
  #:wrap (allocator xmlSchemaFreeParserCtxt))

;; xmlSchemaFree
(define-libxml2 xmlSchemaFree
  (_fun
   _xmlSchema-pointer
   -> _void)
  #:wrap (deallocator))

;; xmlSchemaParse
(define-libxml2 xmlSchemaParse
  (_fun
   _xmlSchemaParserCtxt-pointer
   -> (result : _xmlSchema-pointer)
   -> (check-not-null result "xmlSchemaParse" "cannot parse schema"))
  #:wrap (allocator xmlSchemaFree))

;; xmlSchemaFreeValidCtxt
(define-libxml2 xmlSchemaFreeValidCtxt
  (_fun
   _xmlSchemaValidCtxt-pointer
   -> _void)
  #:wrap (deallocator))

;; xmlSchemaNewValidCtxt
(define-libxml2 xmlSchemaNewValidCtxt
  (_fun
   _xmlSchema-pointer
   -> (result : _xmlSchemaValidCtxt-pointer)
   -> (check-not-null result "xmlSchemaNewValidCtxt" "cannot create schema validation context"))
  #:wrap (allocator xmlSchemaFreeValidCtxt))


;; xmlSchemaValidateDoc
(define-libxml2 xmlSchemaValidateDoc
  (_fun
   _xmlSchemaValidCtxt-pointer
   _xmlDoc-pointer
   -> (result : _int)
   -> (check-valid result "??")))


;;==========================================================
;; Sugar
;;==========================================================


