#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-libxml2 (ffi-lib "libxml2" '("2.9.13" "2" #f)))