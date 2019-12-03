#!r6rs
(import (rnrs))

;;; do
(define do_n 0)
(set! do_n 5)
(do ([i 0 (+ i 1)])
  ((>= i do_n))
  (printf "do, i: ~a~%" i))
