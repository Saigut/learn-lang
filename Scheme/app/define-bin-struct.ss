(import (chezscheme))

(module (
          define-bin-struct
          bin-struct? bin-bytes
          bytevector-copy-sub
          pcap-hdr pcaprec-hdr ethernet-ii-hdr
          ipv4-hdr tcp-hdr
          parser-tcp parser-ipv4
          parse-ethernet-ii parse-pcaprec parse-pcap
          traverse-bin-struct-vector
        )

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
            (not (exists is-not-valid-field? lst)))))
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
                    (define bv-len (bytevector-length bv))
                    (define bits-num (+ eval-bit-width ...))
                    (if (bytevector? bv)
                      (set! data
                        (vector
                          bv
                          (bytevector-uint-ref bv 0 'big bv-len)
                          bv-len
                          bits-num
                          (+ (div bits-num 8)
                            (if (zero? (mod bits-num 8)) 0 1))
                          (lambda (x)
                            (printf "~a:~%" 'name)
                            (printf "~a: 0x~x, " (symbol->string 'field-name) ((vector-ref x index)))
                            ...
                            (printf "~%"))))
                      (error 'constructor "parameter should be bytevector!" bv))
                    (vector 'bin-struct 'name data
                      (lambda ()
                        (let* ([bv-bits-num (* 8 bv-len)]
                                [net-num (bitwise-bit-field (vector-ref data 1)
                                           (- bv-bits-num start-pos eval-bit-width)
                                           (- bv-bits-num start-pos))]
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
                      (vector-set! (vector-ref x 2) 2 bv-len))
                    (error 'set-data-bv "parameter should be bytevector!" bv)))
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

  (define (big-endian-to endian num byte-num)
    (if (eq? endian 'big)
      num
      (let ([bv (uint-list->bytevector
                    `(,num)
                    'big
                  byte-num)])
        (car (bytevector->uint-list
               bv
                 'little
               byte-num)))))

  (define bin-struct?
    (lambda (x)
      (and (vector? x)
        (> (vector-length x) 2)
        (eq? (vector-ref x 0) 'bin-struct)
        (symbol? (vector-ref x 1)))))

  (define (bin-bytes bv base-bv-pos use-bv-sz)
    (cond
      [(not (bytevector? bv))
        (error 'bin-bytes "parameter should be bytevector!" bv)]
      [(not (> (bytevector-length bv) 0))
        (error 'bin-bytes "bytevector size should > 0!" bv)]
      [else
        (lambda ()
          (bytevector-uint-ref bv base-bv-pos 'big use-bv-sz))]))

  (define bytevector-copy-sub
    (lambda (bv start n)
      (if (and (bytevector? bv) (<= (+ start n) (bytevector-length bv)))
        (let ([sub-bv (make-bytevector n)])
          (bytevector-copy! bv start sub-bv 0 n)
          sub-bv)
        (error 'bytevector-copy-sub "invalid parameter!" bv start n (bytevector-length bv)))))

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

  (define (parse-tcp bv base-bv-pos use-bv-sz)
    (define seq-lst '())
    (define cur-bv-offset 0)
    (if (and (< cur-bv-offset use-bv-sz) (>= use-bv-sz tcp-hdr-occupy-byte-num))
      (let ()
        (define tmp-struct (make-tcp-hdr
                             (bytevector-copy-sub bv
                               (+ base-bv-pos cur-bv-offset)
                               tcp-hdr-occupy-byte-num)))
        (set! seq-lst (append! seq-lst `(,tmp-struct)))
        (set! cur-bv-offset (+ cur-bv-offset tcp-hdr-occupy-byte-num))
        (if (< cur-bv-offset use-bv-sz)
          (let ()
            (define tmp-sz (- use-bv-sz cur-bv-offset))
            (set! seq-lst (append! seq-lst `(,(bin-bytes (bytevector-copy-sub bv
                                                           (+ base-bv-pos cur-bv-offset)
                                                           tmp-sz)
                                                0
                                                tmp-sz))))
            (set! cur-bv-offset (+ cur-bv-offset tmp-sz))))
      ))
    (values (list->vector seq-lst) (+ base-bv-pos cur-bv-offset))
  )

  (define (parse-ipv4 bv base-bv-pos use-bv-sz)
    (define seq-lst '())
    (define cur-bv-offset 0)
    (if (and (< cur-bv-offset use-bv-sz) (>= use-bv-sz ipv4-hdr-occupy-byte-num))
      (let ()
        (define tmp-struct (make-ipv4-hdr
                             (bytevector-copy-sub bv
                               (+ base-bv-pos cur-bv-offset)
                               ipv4-hdr-occupy-byte-num)))
        (set! seq-lst (append! seq-lst `(,tmp-struct)))
        (set! cur-bv-offset (+ cur-bv-offset ipv4-hdr-occupy-byte-num))
        (if (< cur-bv-offset use-bv-sz)
          (if (= #x06 (ipv4-hdr-proto tmp-struct))
            (begin
              (let-values ([(ele ret-bv-pos)
                             (parse-tcp bv
                               (+ base-bv-pos cur-bv-offset)
                               (- (ipv4-hdr-total-len tmp-struct) ipv4-hdr-occupy-byte-num))])
                (if (and (> ret-bv-pos (+ base-bv-pos cur-bv-offset))
                      (<= ret-bv-pos (+ base-bv-pos use-bv-sz)))
                  (let ()
                    (set! seq-lst (append! seq-lst `(,ele)))
                    (set! cur-bv-offset (- ret-bv-pos base-bv-pos))))))
            (let ()
              (define tmp-sz (- use-bv-sz cur-bv-offset))
              (set! seq-lst (append! seq-lst `(,(bin-bytes (bytevector-copy-sub bv
                                                             (+ base-bv-pos cur-bv-offset)
                                                             tmp-sz)
                                                  0
                                                  tmp-sz))))
              (set! cur-bv-offset (+ cur-bv-offset tmp-sz)))
          ))
      ))
    (values (list->vector seq-lst) (+ base-bv-pos cur-bv-offset))
  )

  (define (parse-ethernet-ii bv base-bv-pos use-bv-sz)
    (define seq-lst '())
    (define cur-bv-offset 0)
    (if (and (< cur-bv-offset use-bv-sz) (>= use-bv-sz ethernet-ii-hdr-occupy-byte-num))
      (let ()
        (define tmp-struct (make-ethernet-ii-hdr
                             (bytevector-copy-sub bv
                               (+ base-bv-pos cur-bv-offset)
                               ethernet-ii-hdr-occupy-byte-num)))
        (set! seq-lst (append! seq-lst `(,tmp-struct)))
        (set! cur-bv-offset (+ cur-bv-offset ethernet-ii-hdr-occupy-byte-num))
        (if (< cur-bv-offset use-bv-sz)
          (if (= #x0800 (ethernet-ii-hdr-ethertype tmp-struct))
            (begin
              (let-values ([(ele ret-bv-pos)
                             (parse-ipv4 bv
                               (+ base-bv-pos cur-bv-offset)
                               (- use-bv-sz cur-bv-offset))])
                (if (> ret-bv-pos (+ base-bv-pos cur-bv-offset))
                  (if (<= ret-bv-pos (+ base-bv-pos use-bv-sz))
                    (begin
                      (set! seq-lst (append! seq-lst `(,ele)))
                      (set! cur-bv-offset (- ret-bv-pos base-bv-pos))
                      (if (< ret-bv-pos (+ base-bv-pos use-bv-sz))
                        (let ()
                          (define tmp-sz (- (+ base-bv-pos use-bv-sz) ret-bv-pos))
                          (set! seq-lst (append! seq-lst `(,(bin-bytes (bytevector-copy-sub bv
                                                                         (+ base-bv-pos cur-bv-offset)
                                                                         tmp-sz)
                                                              0
                                                              tmp-sz))))
                          (set! cur-bv-offset (+ cur-bv-offset tmp-sz)))))
                    (let ()
                      (define tmp-sz (- use-bv-sz cur-bv-offset))
                      (set! seq-lst (append! seq-lst `(,(bin-bytes (bytevector-copy-sub bv
                                                                     (+ base-bv-pos cur-bv-offset)
                                                                     tmp-sz)
                                                          0
                                                          tmp-sz))))
                      (set! cur-bv-offset (+ cur-bv-offset tmp-sz)))))))
            (let ()
              (define tmp-sz (- use-bv-sz cur-bv-offset))
              (set! seq-lst (append! seq-lst `(,(bin-bytes (bytevector-copy-sub bv
                                                             (+ base-bv-pos cur-bv-offset)
                                                             tmp-sz)
                                                  0
                                                  tmp-sz))))
              (set! cur-bv-offset (+ cur-bv-offset tmp-sz)))
          ))
      ))
    (values (list->vector seq-lst) (+ base-bv-pos cur-bv-offset))
  )

  (define (parse-pcaprec bv base-bv-pos use-bv-sz file-endian)
    (define seq-lst '())
    (define cur-bv-offset 0)
    (define tmp-bv-offset 0)
    (if (and (< tmp-bv-offset use-bv-sz) (>= use-bv-sz pcaprec-hdr-occupy-byte-num))
      (let ()
        (define tmp-struct (make-pcaprec-hdr (bytevector-copy-sub bv (+ base-bv-pos tmp-bv-offset) pcaprec-hdr-occupy-byte-num)))
        (set! seq-lst (append! seq-lst `(,tmp-struct)))
        (set! tmp-bv-offset (+ tmp-bv-offset pcaprec-hdr-occupy-byte-num))
        (let-values ([(ele ret-bv-pos)
                       (parse-ethernet-ii bv
                         (+ base-bv-pos tmp-bv-offset)
                         (big-endian-to file-endian (pcaprec-hdr-incl-len tmp-struct) 4))])
          (if (and (> ret-bv-pos (+ base-bv-pos tmp-bv-offset))
                (<= ret-bv-pos (+ base-bv-pos use-bv-sz)))
            (begin
              (set! seq-lst (append! seq-lst `(,ele)))
              (set! cur-bv-offset (- ret-bv-pos base-bv-pos)))))
      ))
    (values (list->vector seq-lst) (+ base-bv-pos cur-bv-offset))
  )

  (define (parse-pcap bv base-bv-pos use-bv-sz)
    (define seq-lst '())
    (define cur-bv-offset 0)
    (define file-endian 'little)
    (if (and (< cur-bv-offset use-bv-sz) (> use-bv-sz pcap-hdr-occupy-byte-num))
      (let ()
        (define break #f)
        (define tmp-struct (make-pcap-hdr
                             (bytevector-copy-sub
                               bv
                               (+ base-bv-pos cur-bv-offset)
                               pcap-hdr-occupy-byte-num)))
        (set! seq-lst (append! seq-lst `(,tmp-struct)))
        (cond
          [(= (pcap-hdr-magic tmp-struct) #xA1B2C3D4) (set! file-endian 'big)]
          [(= (pcap-hdr-magic tmp-struct) #xD4C3B2A1) (set! file-endian 'little)]
          [else
            (error 'parse-pcap "unsupported pcap file magic!" (pcap-hdr-magic tmp-struct))])
        (set! cur-bv-offset (+ cur-bv-offset pcap-hdr-occupy-byte-num))
        (do ()
          (break)
          (let-values ([(ele ret-bv-pos) (parse-pcaprec bv (+ base-bv-pos cur-bv-offset) (- use-bv-sz cur-bv-offset) file-endian)])
            (if (and (> ret-bv-pos (+ base-bv-pos cur-bv-offset))
                  (<= ret-bv-pos (+ base-bv-pos use-bv-sz)))
              (begin
                (set! seq-lst (append! seq-lst `(,ele)))
                (set! cur-bv-offset (- ret-bv-pos base-bv-pos)))
              (set! break #t))))
      ))
    (values (list->vector seq-lst) (+ base-bv-pos cur-bv-offset))
  )

  (define (traverse-bin-struct-vector v)
    (vector-for-each
      (lambda (x)
        (cond
          [(bin-struct? x)
            ((vector-ref (vector-ref x 2) 5) x)]
          [(procedure? x)
            (printf "0x~x~%" (x))]
          [(vector? x)
            (traverse-bin-struct-vector x)]
          [else
            (error 'traverse-bin-struct-vector "unexpected parameter!" x)]))
      v))
)

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap2.pcap"]
       [fp 0]
       [header 0])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-all fp))

  (let-values ([(v pos)
                 (parse-pcap header 0 (bytevector-length header))])
    (traverse-bin-struct-vector v))

  (close-port fp)
)
