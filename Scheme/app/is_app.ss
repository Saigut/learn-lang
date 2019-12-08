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
                                 (f (cdr lst) (cons (datum->syntax name (/ (cadr (syntax->datum (car lst))) 8)) rst))))
                             )]
                         [(start-pos ...)
                           (let ([cur-pos 0] [tmp-list '()])
                             (let f ([lst #'(field ...)] [rst '()])
                               (if (null? lst)
                                 (reverse rst)
                                 (begin
                                   (set! tmp-list (cons (datum->syntax name cur-pos) rst))
                                   (set! cur-pos (+ cur-pos (/ (cadr (syntax->datum (car lst))) 8)))
                                   (f (cdr lst) tmp-list)))
                               ))])
            #`(begin
                (define (#,name)
                  (define data-bv (make-bytevector 0))
                  (define cur-pos 0)
                  (define endian 'little)
                  (define (field-name)
;                    (printf "my start pos: ~a, my bit width: ~a, my name: ~a~%" start-pos bit-width 'field-name)
                    (bytevector-uint-ref data-bv start-pos endian bit-width))
                  ...
                  (define (set-data-bv bv)
                    (set! data-bv bv))
                  (lambda (msg . params)
                    (cond
                      [(eqv? msg 'field-name) (field-name)]
                      ...
                      [(eqv? msg 'set-data-bv) (set-data-bv (car params))]
                      [else (error 'invalid-message "invalid method!" msg)])))
                ))]
        [_ (syntax-error clause "Invalid fields syntax")]))
    (syntax-case x ()
      [(_ name clause)
        (identifier? #'name)
        (parse-fileds x #'name #'clause)]
      [_ (syntax-error x "Invalid syntax")])))

(define-bin-type cap-hdr
  (fileds
    (magic 32)
    (ver-major 16)
    (ver-minor 16)
    (thiszone 32)
    (sigfigs 32)
    (snaplen 32)
    (network 32)))

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [header 0]
       [pcap-hdr-bin (cap-hdr)])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-n fp 24))

  (pcap-hdr-bin 'set-data-bv header)

  (printf "magic: 0x~x~%" (pcap-hdr-bin 'magic))
  (printf "ver-major: 0x~x~%" (pcap-hdr-bin 'ver-major))
  (printf "ver-minor: 0x~x~%" (pcap-hdr-bin 'ver-minor))
  (printf "thiszone: 0x~x~%" (pcap-hdr-bin 'thiszone))
  (printf "sigfigs: 0x~x~%" (pcap-hdr-bin 'sigfigs))
  (printf "snaplen: 0x~x~%" (pcap-hdr-bin 'snaplen))
  (printf "network: 0x~x~%" (pcap-hdr-bin 'network))

  (close-port fp)
)
