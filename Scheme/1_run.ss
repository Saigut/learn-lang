#!r6rs
(import (rnrs))

(printf "Hello, world!~%")
(printf "Hello, ~a!~%" "string")
(printf "Hello, ~a!~%" 1)
(printf "Hello, 0x~X!~%" 255)
(printf "Hello, 0x~X!~%" #xf)
(printf "Hello, 0x~X!~%" #b11)
(printf "Hello, 0x~X!~%" #o10)