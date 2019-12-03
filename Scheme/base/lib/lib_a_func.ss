#!r6rs
(library (lib lib_a_func)
  (export a_func)
  (import (chezscheme))

  (define a_func
    (lambda ()
      (printf "I am a_func from library lib_a_func~%")))
)
