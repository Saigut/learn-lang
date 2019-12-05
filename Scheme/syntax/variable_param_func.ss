#!r6rs
(import (rnrs))

;; function with variable parameters


;; pass variable parameter to another variable parameter function?
;; answer: the variable parameter is list, so think passing list to a function

(define (name . args)
  ...)

(define (name arg1 . args)
  ...)

(lambda args ...)

(lambda (arg1 . args) ...)