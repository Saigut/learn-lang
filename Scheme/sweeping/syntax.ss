#!r6rs
(import (chezscheme))

;; syntax: (syntax-case expr (literal ...) clause ...)

;(pattern output-expression)
;(pattern fender output-expression)

;; Pattern variables contained within a clause's pattern are bound to the
;; corresponding pieces of the input value within the clause's fender (if
;; present) and output-expression ..... Pattern variables, however, can
;; be referenced only within syntax expressions.

;; fender example (deal-list #'(i ...))
;; it is seems that: (pattern fender output-expression), deal with this form,
;; it will convert #'(i ...) to (#'xx #'xx ...) for fender



(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      [(_ c1 c2 ...)
        (let f ([c1 #'c1] [cmore #'(c2 ...)])
          (if (null? cmore)
            (syntax-case c1 (else =>)
              [(else e1 e2 ...) #'(begin e1 e2 ...)]
              [(e0) #'(let ([t e0]) (if t t))]
              [(e0 => e1) #'(let ([t e0]) (if t (e1 t)))]
              [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))])
            (with-syntax ([rest (f (car cmore) (cdr cmore))])
              (syntax-case c1 (=>)
                [(e0) #'(let ([t e0]) (if t t rest))]
                [(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
                [(e0 e1 e2 ...)
                  #'(if e0 (begin e1 e2 ...) rest)]))))])))

; fake pcar is a variable
(let ([p (cons 0 #f)])
  (define-syntax pcar
    (lambda (x)
      (syntax-case x ()
        [_ (identifier? x) #'(car p)]
        [(_ e) #'(set-car! p e)])))
  (let ([a pcar])
    (pcar 1)
    (list a pcar))) <graphic> (0 1)