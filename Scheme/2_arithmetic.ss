#!r6rs
(import (rnrs))


(define a 0)

(printf "number: 5 3~%")

(set! a (+ 5 3))
(printf "+: ~a~%" a)

(set! a (- 5 3))
(printf "-: ~a~%" a)

(set! a (div 5 3))
(printf "div: ~a~%" a)

(set! a (* 5 3))
(printf "*: ~a~%" a)

;; fixnum:
;; fx+
;; fx-
;; fxdiv
;; fx*