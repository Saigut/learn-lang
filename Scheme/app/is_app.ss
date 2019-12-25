(import (chezscheme))

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
        (and
          (list? lst)
          (if (exists is-not-valid-field? lst)
            #f
            #t))))
    (define (parse-fileds src name clause)
      (syntax-case clause (fileds)
        [(fileds field ...)
          (is-valid-fields? #'(field ...) name)
          (with-syntax ([(field-name ...)
                          (let f ([lst #'(field ...)] [rst '()])
                            (if (null? lst)
                              (reverse rst)
                              (begin
                                (f (cdr lst) (cons (datum->syntax name (car (syntax->datum (car lst)))) rst))))
                            )]
                         [(bit-width ...)
                           (let f ([lst #'(field ...)] [rst '()])
                             (if (null? lst)
                               (reverse rst)
                               (begin
                                 (f (cdr lst) (cons (datum->syntax name (cadr (syntax->datum (car lst)))) rst))))
                             )]
                         [(start-pos ...)
                           (let ([cur-pos 0] [tmp-list '()])
                             (let f ([lst #'(field ...)] [rst '()])
                               (if (null? lst)
                                 (reverse rst)
                                 (begin
                                   (set! tmp-list (cons (datum->syntax name cur-pos) rst))
                                   (set! cur-pos (+ cur-pos (cadr (syntax->datum (car lst)))))
                                   (f (cdr lst) tmp-list)))
                               ))])
            #`(begin
                (define (#,name)
                  (define data-bv (make-bytevector 0))
                  (define data-num 0)
                  (define data-num-bit-width 0)
                  (define (field-name)
;                    (printf "my start pos: ~a, my bit width: ~a, my name: ~a~%" start-pos bit-width 'field-name)
                    (let* ([net-num (bitwise-bit-field data-num
                                     (- data-num-bit-width start-pos bit-width)
                                     (- data-num-bit-width start-pos))]
                            [num-byte-n (+ (div bit-width 8)
                                          (if (zero? (mod bit-width 8))
                                            0 1))]
                            [to-bv (uint-list->bytevector
                                       `(,net-num)
                                       'big
                                     num-byte-n)])
                      (car (bytevector->uint-list
                             to-bv
                             'big
                             num-byte-n)))
                    )
                  ...
                  (define (set-data-bv bv)
                    (set! data-bv bv)
                    (let* ([bv-len (bytevector-length bv)])
                      (set! data-num (bytevector-uint-ref bv 0 'big bv-len))
                      (set! data-num-bit-width (* 8 bv-len))))
                  (define (print-all)
                    (printf "~a: 0x~x~%" (symbol->string 'field-name) (field-name))
                    ...)
                  (lambda (msg . params)
                    (cond
                      [(eqv? msg 'field-name) (field-name)]
                      ...
                      [(eqv? msg 'set-data-bv) (set-data-bv (car params))]
                      [(eqv? msg 'print-all) (print-all)]
                      [else (error 'invalid-message "invalid method!" msg)])))
                ))]
        [_ (syntax-error clause "Invalid fields syntax")]))
    (syntax-case x ()
      [(_ name clause)
        (identifier? #'name)
        (parse-fileds x #'name #'clause)]
      [_ (syntax-error x "Invalid syntax")])))

(define-bin-type cap-file
  (fileds
    (magic 32)
    (ver-major 16)
    (ver-minor 16)
    (thiszone 32)
    (sigfigs 32)
    (snaplen 32)
    (network 32)
    (ts-sec 32)
    (ts-usec 32)
    (incl-len 32)
    (orig-len 32)
    (d-mac-0 8)
    (d-mac-1 8)
    (d-mac-2 8)
    (d-mac-3 8)
    (d-mac-4 8)
    (d-mac-5 8)
    (s-mac-0 8)
    (s-mac-1 8)
    (s-mac-2 8)
    (s-mac-3 8)
    (s-mac-4 8)
    (s-mac-5 8)
    (l3-proto 16)
    (ip-ver 4)
    (ip-hdr-len 4)
    (dscp 6)
    (ecn 2)
    (total-len 16)
    (id 16)
    (flags 3)
    (offset 13)
    (ttl 8)
    (proto 8)
    (checksum 16)
    (sip 32)
    (dip 32)
  ))

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [header 0]
       [pcap-hdr-bin (cap-file)])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-all fp))

  (pcap-hdr-bin 'set-data-bv header)

  (pcap-hdr-bin 'print-all)

  (close-port fp)
)
