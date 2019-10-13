#!r6rs
(import (rnrs))

;; bitwise
(printf "bitwise:~%")

;; and &
(printf "and &, ~bb, ~bb, ~bb~%" #b0010 #b0011 (bitwise-and #b0010 #b0011))

;; or |
(printf "or |, ~bb, ~bb, ~bb~%" #b0010 #b0011 (bitwise-ior #b0010 #b0011))

;; not ~
(printf "not ~~, ~4,'0bb, ~4,'0bb~%" #b0010 (bitwise-and (bitwise-not #b0010) #b1111))

;; xor ^
(printf "xor ^, ~bb, ~bb, ~bb~%" #b0010 #b0011 (bitwise-xor #b0010 #b0011))

;; shift left <<
(printf "shift left <<, ~bb, ~a, ~bb~%" #b00100 2 (bitwise-arithmetic-shift-left #b00100 2))

;; shift right >>
(printf "shift right >>, ~bb, ~a, ~bb~%" #b00100 2 (bitwise-arithmetic-shift-right #b00100 2))


;; fixnum bitwise
(printf "fixnum bitwise:~%")
;; and &
(printf "and &, ~bb, ~bb, ~bb~%" #b0010 #b0011 (fxand #b0010 #b0011))

;; or |
(printf "or |, ~bb, ~bb, ~bb~%" #b0010 #b0011 (fxior #b0010 #b0011))

;; not ~
(printf "not ~~, ~4,'0bb, ~4,'0bb~%" #b0010 (fxand (fxnot #b0010) #b1111))

;; xor ^
(printf "xor ^, ~bb, ~bb, ~bb~%" #b0010 #b0011 (fxxor #b0010 #b0011))

;; shift left <<
(printf "shift left <<, ~bb, ~a, ~bb~%" #b00100 2 (fxarithmetic-shift-left #b00100 2))

;; shift right >>
(printf "shift right >>, ~bb, ~a, ~bb~%" #b00100 2 (fxarithmetic-shift-right #b00100 2))
