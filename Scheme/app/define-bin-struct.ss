(import (chezscheme))

(define-syntax define-bin-struct
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
    (define is-not-valid-field?
      (lambda (stx-lst)
        (let ([ele1 (syntax->datum (car stx-lst))]
               [ele2 (eval (syntax->datum (cadr stx-lst)))])
          (not (and
                 (symbol? ele1)
                 (integer? ele2)
                 (> ele2 0))))))
    (define is-valid-fields?
      (lambda (lst)
        (and
          (list? lst)
          (not (null? lst))
          (if (exists is-not-valid-field? lst)
            #f
            #t))))
    (syntax-case x (fields)
      [(k name (fields (field-name bit-width) ...))
        (is-valid-fields? #'((field-name bit-width) ...))
        (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                       [(start-pos ...)
                           (let ([cur-pos 0] [tmp-list '()])
                             (let f ([lst #'(bit-width ...)] [rst '()])
                               (if (null? lst)
                                 (reverse rst)
                                 (begin
                                   (set! tmp-list (cons (datum->syntax #'name cur-pos) rst))
                                   (set! cur-pos (+ cur-pos (eval (syntax->datum (car lst)))))
                                   (f (cdr lst) tmp-list)))
                             ))]
                       [predicate (gen-id #'name #'name "?")]
                       [(access ...)
                         (map (lambda (x) (gen-id #'name #'name "-" x))
                           #'(field-name ...))]
                       [(eval-bit-width ...) (map
                                               (lambda (x) (eval (syntax->datum x)))
                                               #'(bit-width ...))]
                       [set-data-bv
                         (gen-id #'name #'name "-set-data-bv")]
                       [print-all
                         (gen-id #'name #'name "-print-all")]
                       [bit-size
                         (gen-id #'name #'name "-bit-size")]
                       [occupy-byte-num
                         (gen-id #'name #'name "-occupy-byte-num")]
                       [structure-length (+ (length #'(field-name ...)) 3)]
                       [(index ...)
                         (let f ([i 3] [ids #'(field-name ...)])
                           (if (null? ids)
                               '()
                             (cons i (f (+ i 1) (cdr ids)))))])
          #'(begin
              (define constructor
                (lambda (bv)
                  (define data 0)
                  (define bits-num (+ eval-bit-width ...))
                  (if (bytevector? bv)
                    (let ([bv-len (bytevector-length bv)])
                      (set! data
                        (vector
                          bv
                          (bytevector-uint-ref bv 0 'big bv-len)
                          (* 8 bv-len)
                          bits-num
                          (+ (div bits-num 8)
                             (if (zero? (mod bits-num 8)) 0 1)))))
                    (error 'invalid-parameter "parameter should be bytevector!" bv))
                  (vector 'bin-struct 'name data
                    (lambda ()
                      (let* ([net-num (bitwise-bit-field (vector-ref data 1)
                                        (- (vector-ref data 2) start-pos eval-bit-width)
                                        (- (vector-ref data 2) start-pos))]
                              [num-byte-n (+ (div eval-bit-width 8)
                                            (if (zero? (mod eval-bit-width 8)) 0 1))]
                              [to-bv (uint-list->bytevector
                                       `(,net-num)
                                       'big
                                        num-byte-n)])
                      (car (bytevector->uint-list
                             to-bv
                             'big
                             num-byte-n))))
                    ...)))
              (define predicate
                (lambda (x)
                  (and (vector? x)
                    (= (vector-length x) structure-length)
                    (eq? (vector-ref x 0) 'bin-struct)
                    (eq? (vector-ref x 1) 'name))))
              (define access
                (lambda (x)
                  ((vector-ref x index))))
              ...
              (define (set-data-bv x bv)
                (if (bytevector? bv)
                  (let ([bv-len (bytevector-length bv)])
                    (vector-set! (vector-ref x 2) 0 bv)
                    (vector-set! (vector-ref x 2) 1 (bytevector-uint-ref bv 0 'big bv-len))
                    (vector-set! (vector-ref x 2) 2 (* 8 bv-len)))
                  (error 'invalid-parameter "parameter should be bytevector!" bv)))
              (define (print-all x)
                (printf "~a: 0x~x~%" (symbol->string 'field-name) (access x))
                ...)
              (define bit-size
                (+ eval-bit-width ...))
              (define occupy-byte-num
                (let ([bits-num (+ eval-bit-width ...)])
                  (+ (div bits-num 8)
                    (if (zero? (mod bits-num 8)) 0 1))))))]
      [_ (syntax-error x "Invalid syntax")])))

(define bin-struct?
  (lambda (x)
    (and (vector? x)
      (> (vector-length x) 2)
      (eq? (vector-ref x 0) 'bin-struct)
      (symbol? (vector-ref x 1)))))

(define (bin-bytes bv)
  (cond
    [(not (bytevector? bv))
      (error 'invalid-parameter "parameter should be bytevector!" bv)]
    [(not (> (bytevector-length bv) 0))
      (error 'invalid-parameter "bytevector size should > 0!" bv)]
    [else
      (lambda ()
        (bytevector-uint-ref bv 0 'big (bytevector-length bv)))]))

(define bytevector-copy-sub
  (lambda (bv start n)
    (if (and (bytevector? bv) (<= (+ start n) (bytevector-length bv)))
      (let ([sub-bv (make-bytevector n)])
        (bytevector-copy! bv start sub-bv 0 n)
        sub-bv)
      (error 'invalid-parameter "invalid parameter!" bv start n))))

(define-bin-struct pcap-hdr
  (fields
    (magic 32)
    (ver-major 16)
    (ver-minor 16)
    (thiszone 32)
    (sigfigs 32)
    (snaplen 32)
    (network 32)))

(define-bin-struct pcaprec-hdr
  (fields
    (ts-sec 32)
    (ts-usec 32)
    (incl-len 32)
    (orig-len 32)))

(define-bin-struct ethernet-ii-hdr
  (fields
    (dmac (* 6 8))
    (smac (* 6 8))
    (ethertype 16)))

(define-bin-struct ipv4-hdr
  (fields
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
    (dip 32)))

(define-bin-struct tcp-hdr
  (fields
    (sport 16)
    (dport 16)
    (seq-num 32)
    (ack-num 32)
    (offset 4)
    (reserved 3)
    (flags 9)
    (windows-sz 16)
    (checksum 16)
    (urgent-pointer 16)))

(define (parse-pcap bv)
  (define seq-lst '())
  (define bv-byte-num (bytevector-length bv))
  (define cur-bv-pos 0)
  (if (> bv-byte-num pcap-hdr-occupy-byte-num)
    (begin
      (append seq-lst `(,(make-pcap-hdr (bytevector-copy-sub bv 0 pcap-hdr-occupy-byte-num))))
      (set! cur-bv-pos (+ cur-bv-pos pcap-hdr-occupy-byte-num))
      (do ([i 0])
        ((>= cur-bv-pos bv-byte-num))
        (printf "do, i: ~a~%" i))
    ))
  seq-lst
)

(define-bin-struct cap-file
  (fields
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
       [header 0])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-all fp))

  (let ([bin (make-cap-file header)])
    (cap-file-print-all bin))

  (close-port fp)
)
