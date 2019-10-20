#!r6rs
(import (rnrs))

;; string
(printf "type string: ~a~%" "string")

;; character
;; #\newline #\space #\tab  Unicode: #\xHEX
(printf "type character: ~a ~a ~a~%" #\a #\b #\d)

;; integer
(printf "type integer: ~a~%" 22222)

;; float
(printf "type float: ~a~%" 1.23345)

;; symbol
(printf "type symbol: ~a~%" 'symbol)
