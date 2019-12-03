#!r6rs
(import (rnrs))

;;; if
(printf "if false~%")
(if #f
  (printf "then true~%")
  (printf "then false~%"))

(printf "if true~%")
(if #t
  (printf "then true~%")
  (printf "then false~%"))

;;; when
(when #t
  (printf "when~%"))

;;; unless
(unless #f
  (printf "unless~%"))

;;; cond
(define cond_val 0)
(define cond_result "")

(set! cond_val 1)
(printf "cond, number: ~a~%" cond_val)
(set! cond_result
  (cond
    ((= cond_val 1) "1")
    ((= cond_val 2) "2")
    (else "else")))
(printf "cond_result: ~a~%" cond_result)

(set! cond_val 2)
(printf "cond, number: ~a~%" cond_val)
(set! cond_result
  (cond
    ((= cond_val 1) "1")
    ((= cond_val 2) "2")
    (else "else")))
(printf "cond_result: ~a~%" cond_result)

(set! cond_val 3)
(printf "cond, number: ~a~%" cond_val)
(set! cond_result
  (cond
    ((= cond_val 1) "1")
    ((= cond_val 2) "2")
    (else "else")))
(printf "cond_result: ~a~%" cond_result)

;;; case
(define case_val 0)
(define case_result "")

(set! case_val 1)
(printf "case, number: ~a~%" case_val)
(set! case_result
  (case case_val
    ((1) "1")
    ((2) "2")
    (else "else")))
(printf "case_result: ~a~%" case_result)

(set! case_val 2)
(printf "case, number: ~a~%" case_val)
(set! case_result
  (case case_val
    ((1) "1")
    ((2) "2")
    (else "else")))
(printf "case_result: ~a~%" case_result)

(set! case_val 3)
(printf "case, number: ~a~%" case_val)
(set! case_result
  (case case_val
    ((1) "1")
    ((2) "2")
    (else "else")))
(printf "case_result: ~a~%" case_result)
