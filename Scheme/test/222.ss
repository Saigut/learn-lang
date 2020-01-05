#!r6rs
(import (chezscheme))

(let [(rst (member
               'xxx
             (environment-symbols (interaction-environment))))]
  (if (pair? rst)
    (display (car rst))
    (display rst)))

(define (check-var-env var)
  (let [(rst (member
               var
               (environment-symbols (interaction-environment))))]
    (if (pair? rst)
      (printf "~a~%" (car rst))
      (printf "~a~%" rst))))

(let ()
  (define xxx 111)
  (check-var-env 'xxx)
  xxx)

(define-syntax def-new-type
  (lambda (x)
    (syntax-case x ()
      [(_ name body)
        #'(define name "just-it")])))

(define-syntax def-new-type
  (lambda (x)
    (define (parser-body name body)
      (syntax-case body (bd)
        [(bd other ...)
          #'(define xxxname "just-it-")]))
    (syntax-case x ()
      [(_ name body)
        #'(define xxxname "just-it-")])))

(def-new-type bbbbb (bd 111))

(let ([bv #vu8(#x01 #x02 #x03 #x04)])
  (define n (bytevector-uint-ref bv 0 'big (bytevector-length bv)))
  (printf "bv: ~b~%" bv)
  (printf "num bigend: ~x~%" n)
  (printf "num bigend reverse: ~x~%" (bitwise-reverse-bit-field n 0 (* 8 (bytevector-length bv))))
)

#;(and (integer? (syntax->datum #'num)) )

(define-syntax make-v-wrap
  (lambda (x)
    (syntax-case x ()
      [(k name n)
        (with-syntax ([tmp-name (datum->syntax #'k (gensym))])
          #'(begin
              (define tmp-name (make-vector n))
              (define-syntax name
                (syntax-rules ()
                  [(_) tmp-name]
                  [(_ idx) (vector-ref tmp-name idx)]))))])))
;; (make-v-wrap v 10)
;; (v)
;; (v 2)
;; (vector-set! (v) 2 10)
;; (v 2)


(define-syntax make-v-wrap
  (lambda (x)
    (syntax-case x ()
      [(k name n)
        #'(begin
            (define-syntax name
              (let ([the-v (make-vector n)])
                (syntax-rules ()
                  [(_) the-v]
                  [(_ idx) (vector-ref the-v idx)]))))])))
;; (make-v-wrap v 10)
;; (v)
;; Exception: attempt to reference out-of-phase identifier the-v

(define-syntax make-v-wrap
  (lambda (x)
    (syntax-case x ()
      [(k name n)
        #'(begin
            (define name
              (let ([the-v (make-vector n)])
                (case-lambda
                  [() the-v]
                  [(idx) (vector-ref the-v idx)]
                  [(idx new-val) (vector-set! the-v idx new-val)]))))])))
;; (make-v-wrap v 10)
;; (v)
;; #(0 0 0 0 0 0 0 0 0 0)
;; (v 1)
;; 0
;; (v 1 333)
;; (v)
;; #(0 333 0 0 0 0 0 0 0 0)
