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
          #'(define name "just-it-")]))
    (syntax-case x ()
      [(_ name body)
        (parser-body #'name #'body)])))