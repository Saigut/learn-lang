#!r6rs
(import (chezscheme))

(let-values ([(a b) (values 1 2)]
              [c (values 1 2 3)])
  (list a b c))


;; The procedure values accepts any number of arguments and simply passes (returns) the arguments to its continuation.
(values 1 2 3)
; 1
; 2
; 3
(call-with-values
  (lambda () (values 1 2 3))
  list)
(call-with-values
  (lambda () (values 1 2 3))
  define-values)


(call/cc)

(+ 1 2)
(+ (call/cc (lambda (k) (k 3))) 2)