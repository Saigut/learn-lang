#!r6rs
(import (chezscheme))

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [bv 0]
       [magic 0]
       [ver-major 0]
       [ver-minor 0]
       [thiszone 0]
       [sigfigs 0]
       [snaplen 0]
       [network 0]
       )
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
;  (set! bv (get-bytevector-all fp))
;  (printf "~X" bv)
  (set! magic (get-bytevector-n fp 4))
  (set! ver-major (get-bytevector-n fp 2))
  (set! ver-minor (get-bytevector-n fp 2))
  (set! thiszone (get-bytevector-n fp 4))
  (set! sigfigs (get-bytevector-n fp 4))
  (set! snaplen (get-bytevector-n fp 4))
  (set! network (get-bytevector-n fp 4))

  (printf "magic: ~x~%" magic)
  (printf "ver-major: ~x~%" ver-major)
  (printf "ver-minor: ~x~%" ver-minor)
  (printf "thiszone: ~x~%" thiszone)
  (printf "sigfigs: ~x~%" sigfigs)
  (printf "snaplen: ~x~%" snaplen)
  (printf "network: ~x~%" network)

  (close-port fp)
)

(define-syntax define-bin-record-type
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax
          template-identifier
          (string->symbol
            (apply string-append
              (map (lambda (x)
                     (if (string? x)
                       x
                       (symbol->string (syntax->datum x))))
                args))))))
    (define (do-define-bin-record-type src name make-name pred-name clause*)
      "nothing")
    (syntax-case x ()
      [(_ name clause ...)
        (identifier? #'name)
        (do-define-bin-record-type x #'name
          (construct-name #'name "make-" #'name)
          (construct-name #'name #'name "?")
          #'(clause ...))])))

(let ()
  (define (test-define1) "111")
  "nothing"
  )

(define-syntax make-environment
  (syntax-rules ()
    ((_ definition ...)
      (let ((environment (scheme-report-environment 5)))
        (eval '(begin definition
                 ...)
          environment)
        environment))))

(define define-it
  (make-environment
    (define test-define-sb22 "111")))

(define-syntax define-class
  (lambda (x)
    (syntax-case x ()
      [(_ name)
        (with-syntax ([method1 (datum->syntax
                               #'name
                               (string->symbol
                                 (string-append
                                   "class-"
                                   (symbol->string (syntax->datum #'name))
                                   "-method1")))]
                       [method2 (datum->syntax
                               #'name
                               (string->symbol
                                 (string-append
                                   "class-"
                                   (symbol->string (syntax->datum #'name))
                                   "-method2")))])
          #'(begin
              (define method1 (lambda () "nothing"))
              (define method2 (lambda () "nothing"))))])))

(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
          (string->symbol
            (apply string-append
              (map (lambda (x)
                     (if (string? x)
                       x
                       (symbol->string (syntax->datum x))))
                args))))))
    (syntax-case x ()
      [(_ name field ...)
        (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                       [predicate (gen-id #'name #'name "?")]
                       [(access ...)
                         (map (lambda (x) (gen-id x #'name "-" x))
                           #'(field ...))]
                       [(assign ...)
                         (map (lambda (x)
                                (gen-id x "set-" #'name "-" x "!"))
                           #'(field ...))]
                       [structure-length (+ (length #'(field ...)) 1)]
                       [(index ...)
                         (let f ([i 1] [ids #'(field ...)])
                           (if (null? ids)
                               '()
                             (cons i (f (+ i 1) (cdr ids)))))])
          #'(begin
              (define constructor
                (lambda (field ...)
                  (vector 'name field ...)))
              (define predicate
                (lambda (x)
                  (and (vector? x)
                    (= (vector-length x) structure-length)
                    (eq? (vector-ref x 0) 'name))))
              (define access
                (lambda (x)
                  (vector-ref x index)))
              ...
              (define assign
                (lambda (x update)
                  (vector-set! x index update)))
              ...))])))

(define-syntax define-it
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
          (string->symbol
            (apply string-append
              (map (lambda (x)
                     (if (string? x)
                       x
                       (symbol->string (syntax->datum x))))
                args))))))
    (syntax-case x ()
      [(_ name field ...)
        (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                       [predicate (gen-id #'name #'name "?")]
                       [(access ...)
                         (map (lambda (x) (gen-id x #'name "-" x))
                           #'(field ...))]
                       [(assign ...)
                         (map (lambda (x)
                                (gen-id x "set-" #'name "-" x "!"))
                           #'(field ...))]
                       [structure-length (+ (length #'(field ...)) 1)]
                       [(index ...)
                         (let f ([i 1] [ids #'(field ...)])
                           (if (null? ids)
                               '()
                             (cons i (f (+ i 1) (cdr ids)))))]
                       [test-define-dasb (gen-id #'name "test-define-dasb")])
          #'(begin
              (define constructor
                (lambda (field ...)
                  (vector 'name field ...)))
              (define predicate
                (lambda (x)
                  (and (vector? x)
                    (= (vector-length x) structure-length)
                    (eq? (vector-ref x 0) 'name))))
              (define access
                (lambda (x)
                  (vector-ref x index)))
              ...
              (define assign
                (lambda (x update)
                  (vector-set! x index update)))
              ...
              (define test-define-dasb
                (lambda (x) "111"))))])))