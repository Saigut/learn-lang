#!r6rs
(import (rnrs))

(define a "")

(set! a "hello")
(printf "string: ~a~%" a)

(let ([b "world"])
  (printf "string: ~a~%" b))