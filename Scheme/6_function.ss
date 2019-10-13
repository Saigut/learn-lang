#!r6rs
(import (rnrs))

(define func
  (lambda ()
    (printf "I am a function!!~%")))

(printf "call function:~%")
(func)