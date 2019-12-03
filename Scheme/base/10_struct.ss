#!r6rs
(import (rnrs))


;(define-record-type a_struct_t (fields s b))
(define-record-type a_struct_t
  (fields (mutable s) (mutable b))
  (protocol
    (lambda (new)
      (lambda (s)
        (new s "original-b")))))

(define-record-type (a_struct2_t constructor-name predicate-name)
  (fields (mutable s) (mutable b))
  (protocol
    (lambda (new)
      (lambda (s)
        (new s "original-b")))))

;(make-a_struct_t x y)	constructor
;(a_struct_t? obj)	predicate
;(a_struct_t-s obj)	accessor for field x
;(a_struct_t-b obj)	accessor for field y
;(a_struct_t-s-set! obj xxx)   only for
;(a_struct_t-b-set! obj xxx)


(define a_struct (make-a_struct_t "sss"))
(printf "1. s: ~a, b: ~a~%" (a_struct_t-s a_struct) (a_struct_t-b a_struct))

(set! a_struct (make-a_struct_t "sss" "bbb"))
(printf "1. s: ~a, b: ~a~%" (a_struct_t-s a_struct) (a_struct_t-b a_struct))

(a_struct_t-s-set! a_struct "sss+++")
(a_struct_t-b-set! a_struct "bbb+++")
(printf "2. s: ~a, b: ~a~%" (a_struct_t-s a_struct) (a_struct_t-b a_struct))
