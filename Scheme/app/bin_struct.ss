(import (chezscheme))


(define-bin-type cap-hdr
  (fileds
    (magic 32)
    (ver-major 16)
    (ver-minor 16)
    (thiszone 32)
    (sigfigs 32)
    (snaplen 32)
    (network 32)))

(define-bin-type cap-hdr
  (fileds
    magix
    bbb))

(define-syntax define-bin-type
  (lambda (x)
    (define is-not-valid-field?
      (lambda (lst-stx)
        (let ([lst (syntax->datum lst-stx)])
          (not (and
                 (list? lst)
                 (= 2 (length lst))
                 (symbol? (car lst))
                 (integer? (cadr lst))
                 (> (cadr lst) 0))))))
    (define is-valid-fields?
      (lambda (lst name)
        (printf "~a~%" lst)
        (printf "!!!name: ~a~%" name)
        (and
          (list? lst)
          (if (exists is-not-valid-field? lst)
            #f
            #t))))
    (define (parse-fileds src name clause)
      (syntax-case clause (fileds)
        [(fileds field ...)
          (is-valid-fields? #'(field ...) name)
          (with-syntax ([(field-name ...) (map
                                            (lambda (x)
                                              (printf "x: ~a~%" x)
                                              (printf "(syntax->datum x): ~a~%" (syntax->datum x))
                                              (printf "(car (syntax->datum x)): ~a~%" (car (syntax->datum x)))
                                              (datum->syntax name (car (syntax->datum x))))
                                            #'(field ...))])
            #`(begin
                (define #,name #,x)
                (define setx #,_setx)
                (define getx #,_getx)
                  #;(define-record-type #,name
                      (fields (mutable field-name) ...))))]
        [_ (syntax-error clause "Invalid fields syntax")]))
    (syntax-case x ()
      [(_ name clause)
        (identifier? #'name)
        (parse-fileds x #'name #'clause)]
      [_ (syntax-error x "Invalid syntax")])))

(define-syntax define-values
  (lambda (x)
    (define is-not-valid-pair?
      (lambda (lst-stx)
        (let ([lst (syntax->datum lst-stx)])
          (not (and
                 (list? lst)
                 (= 2 (length lst))
                 (symbol? (car lst)))))))
    (define is-valid-pairs?
      (lambda (lst)
        (and
          (list? lst)
          (if (exists is-not-valid-pair? lst)
            #f
            #t))))
    (syntax-case x ()
      [(macro-name pair ...)
        (is-valid-pairs? #'(pair ...))
        (with-syntax ([(name ...) (map
                                    (lambda (x)
                                      (printf "x: ~a~%" x)
                                      (printf "(syntax->datum x): ~a~%" (syntax->datum x))
                                      (printf "(car (syntax->datum x)): ~a~%" (car (syntax->datum x)))
                                      (datum->syntax #'macro-name (car (syntax->datum x))))
                                    #'(pair ...))]
                       [(value ...) (map
                                      (lambda (x)
                                      (printf "x: ~a~%" x)
                                      (printf "(syntax->datum x): ~a~%" (syntax->datum x))
                                      (printf "(cadr (syntax->datum x)): ~a~%" (cadr (syntax->datum x)))
                                      (datum->syntax #'macro-name (cadr (syntax->datum x))))
                                    #'(pair ...))])
          #'(begin
              (define name value)
              ...))]
      [_ (syntax-error x "Invalid syntax")])))

(define-values (aaa "aaa") (my+ +))

(define define-values-pro
  (lambda x))

(define-syntax define-bin-type
  (lambda (x)
    (define is-not-valid-field?
      (lambda (lst-stx)
        (let ([lst (syntax->datum lst-stx)])
          (not (and
                 (list? lst)
                 (= 2 (length lst))
                 (symbol? (car lst))
                 (integer? (cadr lst))
                 (> (cadr lst) 0))))))
    (define is-valid-fields?
      (lambda (lst)
        (printf "~a~%" lst)
        (and
          (list? lst)
          (if (exists is-not-valid-field? lst)
            #f
            #t))))
    (syntax-case x (fileds)
      [(_ name (fileds field ...))
        (is-valid-fields? #'(field ...))
        #'(printf "~a~%" '(field ...))])))


(define-syntax define-bin-type
  (lambda (x)
    (syntax-case x (fileds)
      [(_ name (fileds field ...))
        #'#f])))

(define-syntax or22
  (lambda (x)
    (syntax-case x ()
      [(_) #'#f]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
        #'(let ([t e1]) (if t t (or e2 e3 ...)))])))

(define-syntax define-bin-type
  (lambda (x)
    (define is-not-valid-field?
      (lambda (lst-stx)
        (let ([lst (syntax->datum lst-stx)])
          (not (and
                 (list? lst)
                 (= 2 (length lst))
                 (symbol? (car lst))
                 (integer? (cadr lst))
                 (> (cadr lst) 0))))))
    (define is-valid-fields?
      (lambda (lst name)
        (printf "~a~%" lst)
        (printf "!!!name: ~a~%" name)
        (and
          (list? lst)
          (if (exists is-not-valid-field? lst)
            #f
            #t))))
    (define (parse-fileds src name clause)
      (syntax-case clause (fileds)
        [(fileds field ...)
          (is-valid-fields? #'(field ...) name)
          (with-syntax ([(field-name ...) (map
                                            (lambda (x)
                                              (printf "x: ~a~%" x)
                                              (printf "(syntax->datum x): ~a~%" (syntax->datum x))
                                              (printf "(car (syntax->datum x)): ~a~%" (car (syntax->datum x)))
                                              (datum->syntax name (car (syntax->datum x))))
                                            #'(field ...))])
            #`(begin
                (define (#,name)
                  (define data-bv (make-bytevector 0))
                  (define cur-pos 0)
                  (define endian 'big)
                  (define (get-magic) (bytevector-uint-ref data-bv 0 endian 4))
                  (define (get-ver-major) (bytevector-uint-ref data-bv 4 endian 2))
                  (define (get-ver-minor) (bytevector-uint-ref data-bv 8 endian 2))
                  (define (set-data-bv bv)
                    (set! data-bv bv)
                    (let ([magic (bytevector-uint-ref data-bv 0 'big 4)])
                      (cond
                        ((= magic (bytevector-uint-ref #vu8(#xa1 #xb2 #xc3 #xd4) 0 'big 4)) (set! endian 'big))
                        ((= magic (bytevector-uint-ref #vu8(#xa1 #xb2 #xc3 #xd4) 0 'little 4)) (set! endian 'little))
                        (else (error 'invalid-magic "invalid magic!" magic)))))
                  (lambda (msg . params)
                    (cond
                      [(eqv? msg 'get-magic) (get-magic)]
                      [(eqv? msg 'get-ver-major) (get-ver-major)]
                      [(eqv? msg 'get-ver-minor) (get-ver-minor)]
                      [(eqv? msg 'set-data-bv) (set-data-bv (car params))]
                      [else (error 'invalid-message "invalid method!" msg)])))
                  #;(define-record-type #,name
                      (fields (mutable field-name) ...))))]
        [_ (syntax-error clause "Invalid fields syntax")]))
    (syntax-case x ()
      [(_ name clause)
        (identifier? #'name)
        (parse-fileds x #'name #'clause)]
      [_ (syntax-error x "Invalid syntax")])))

(define (new-bin-struct)
  (define data-bv (make-bytevector 0))
  (define cur-pos 0)
  (define endian 'big)
  (define (get-magic) (bytevector-uint-ref data-bv 0 endian 4))
  (define (get-ver-major) (bytevector-uint-ref data-bv 4 endian 2))
  (define (get-ver-minor) (bytevector-uint-ref data-bv 8 endian 2))
  (define (set-data-bv bv)
    (set! data-bv bv)
    (let ([magic (bytevector-uint-ref data-bv 0 'big 4)])
      (cond
        ((= magic (bytevector-uint-ref #vu8(#xa1 #xb2 #xc3 #xd4) 0 'big 4)) (set! endian 'big))
        ((= magic (bytevector-uint-ref #vu8(#xa1 #xb2 #xc3 #xd4) 0 'little 4)) (set! endian 'little))
        (else (error 'invalid-magic "invalid magic!" magic)))))
  (lambda (msg . params)
    (cond
      [(eqv? msg 'get-magic) (get-magic)]
      [(eqv? msg 'get-ver-major) (get-ver-major)]
      [(eqv? msg 'get-ver-minor) (get-ver-minor)]
      [(eqv? msg 'set-data-bv) (set-data-bv (car params))]
      [else (error 'invalid-message "invalid method!" msg)])))

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [header 0]
       [pcap-hdr-bin (new-bin-struct)])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-n fp 24))

  (pcap-hdr-bin 'set-data-bv header)

  (printf "magic: ~x~%" (pcap-hdr-bin 'get-magic))
  (printf "ver-major: ~x~%" (pcap-hdr-bin 'get-ver-major))
  (printf "ver-minor: ~x~%" (pcap-hdr-bin 'get-ver-minor))

  (close-port fp)
)

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [header 0]
       [pcap-hdr-bin (cap-hdr)])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-n fp 24))

  (pcap-hdr-bin 'set-data-bv header)

  (printf "magic: ~x~%" (pcap-hdr-bin 'get-magic))
  (printf "ver-major: ~x~%" (pcap-hdr-bin 'get-ver-major))
  (printf "ver-minor: ~x~%" (pcap-hdr-bin 'get-ver-minor))

  (close-port fp)
)
