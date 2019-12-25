#!r6rs
(import (rnrs))


;; data, or function, or group of data or function.
;; is it convenient to check is it what I want.

(module mod_name (aaa bbb)
  (define aaa 'aaa)
  (define bbb 'bbb))

(import mod_name)

(let ()
  (import-only mod_name))

(module (ccc ddd)
  (define ccc 'ccc)
  (define ddd 'ddd))

(module np-why ()
  (export np-why1 np-why2 np-why3)
  (define np-why1 'np-why1)
  (define np-why2 'np-why2)
  (define np-why3 'np-why3))

(module np-why ()
  (define np-why1 'np-why1)
  (define np-why2 'np-why2)
  (define np-why3 'np-why3))

(module np-why (np-why1 np-why2 np-why3)
  (define np-why1 'np-why1)
  (define np-why2 'np-why2)
  (define np-why3 'np-why3))

(import np-why)

(display "Hello, world!")