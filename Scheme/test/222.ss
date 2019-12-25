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
