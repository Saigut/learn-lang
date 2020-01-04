(import (chezscheme))


;; bv-info: (vector (vector bv cur-bv-pos use-bv-sz just-used) rst-v)
(define get-bv
  (lambda (bv-info)
    (vector-ref (vector-ref bv-info 0) 0)))

(define get-bv-cur-pos
  (lambda (bv-info)
    (vector-ref (vector-ref bv-info 0) 1)))

(define set-bv-cur-pos
  (lambda (bv-info cur-pos)
    (vector-set! (vector-ref bv-info 0) 1 cur-pos)))

(define get-bv-use-sz
  (lambda (bv-info)
    (vector-ref (vector-ref bv-info 0) 2)))

(define set-bv-use-sz
  (lambda (bv-info bv-use-sz)
    (vector-set! (vector-ref bv-info 0) 2 bv-use-sz)))

(define get-bv-just-used
  (lambda (bv-info)
    (vector-ref (vector-ref bv-info 0) 3)))

(define set-bv-just-used
  (lambda (bv-info just-used)
    (vector-set! (vector-ref bv-info 0) 3 just-used)))

(define get-rst-v
  (lambda (bv-info)
    (vector-ref bv-info 1)))

(define set-rst-v
  (lambda (bv-info rst-v)
    (vector-set! bv-info 1 rst-v)))

(define parser-def-helper
  (lambda (bv-info make-st st-sz)
    (define bv-wrap (vector-ref bv-info 0))
    (define bv (vector-ref bv-wrap 0))
    (define cur-bv-pos (vector-ref bv-wrap 1))
    (define use-bv-sz (get-bv-use-sz bv-info))
    (if (>= use-bv-sz st-sz)
      (values
        (make-st
          (bytevector-copy-sub bv
            cur-bv-pos
            st-sz))
        st-sz)
      (values #f 0))))

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
                       [type-name
                         (gen-id #'name #'name "-type")]
                       [structure-length (+ (length #'(field-name ...)) 3)]
                       [parser-name (gen-id #'name "parse-" #'name)]
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
                          (printf "~%"))
                        (+ eval-bit-width ...)
                        (let ([bits-num (+ eval-bit-width ...)])
                          (+ (div bits-num 8)
                            (if (zero? (mod bits-num 8)) 0 1)))))
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
                    (if (zero? (mod bits-num 8)) 0 1))))
              (define parser-name
                (lambda (bv-info)
                  (parser-def-helper bv-info constructor occupy-byte-num)))
              (define name
                (lambda ()
                  (define data
                    (vector
                      (+ eval-bit-width ...)
                      (let ([bits-num (+ eval-bit-width ...)])
                        (+ (div bits-num 8)
                          (if (zero? (mod bits-num 8)) 0 1)))))
                  (vector 'bin-struct-type 'type-name data)))
              ))]
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

(define bin-struct-type?
  (lambda (x)
    (and (vector? x)
      (= (vector-length x) 3)
      (eq? (vector-ref x 0) 'bin-struct-type)
      (symbol? (vector-ref x 1))
      (let ([v (vector-ref x 2)])
        (and (vector? v) (= 2 (vector-length v)))))))

(define bin-struct-type-bit-size
  (lambda (x)
    ((vector-ref (vector-ref x 2) 0) x)))

(define bin-struct-type-occupy-byte-num
  (lambda (x)
    ((vector-ref (vector-ref x 2) 1) x)))

(define bin-struct?
  (lambda (x)
    (and (vector? x)
      (> (vector-length x) 3)
      (eq? (vector-ref x 0) 'bin-struct)
      (symbol? (vector-ref x 1))
      (let ([v (vector-ref x 2)])
        (and (vector? v) (= 8 (vector-length v)))))))

(define bin-struct-print-all
  (lambda (x)
    ((vector-ref (vector-ref x 2) 5) x)))

(define bin-struct-bit-size
  (lambda (x)
    ((vector-ref (vector-ref x 2) 6) x)))

(define bin-struct-occupy-byte-num
  (lambda (x)
    ((vector-ref (vector-ref x 2) 7) x)))

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

(define (traverse-bin-struct-vector v)
  (vector-for-each
    (lambda (x)
      (traverse-bin-struct x))
    v))

(define (traverse-bin-struct x)
  (cond
    [(bin-struct? x)
      ((vector-ref (vector-ref x 2) 5) x)]
    [(procedure? x)
      (printf "0x~x~%" (x))]
    [(vector? x)
      (traverse-bin-struct-vector x)]
    [else
      (error 'traverse-bin-struct "unexpected parameter!" x)]))

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


(define parse-bytes
  (lambda (bv-info)
    (define bv-wrap (vector-ref bv-info 0))
    (define bv (vector-ref bv-wrap 0))
    (define cur-bv-pos (vector-ref bv-wrap 1))
    (define use-bv-sz (get-bv-use-sz bv-info))
    (values
      (bin-bytes
        bv
        cur-bv-pos
        use-bv-sz)
      use-bv-sz)))

;; one paresr deal with exact one fix size binary struct

(define bin-parser-holder
  (lambda (parser bv-info)
    (define bv-wrap (vector-ref bv-info 0))
    (define bv (vector-ref bv-wrap 0))
    (define cur-bv-pos (vector-ref bv-wrap 1))
    (define rst-v (vector-ref bv-info 1))
    (define use-bv-sz (get-bv-use-sz bv-info))
    (if (and (> use-bv-sz 0) (>= cur-bv-pos 0)
          (<= (+ cur-bv-pos use-bv-sz) (bytevector-length bv)))
      (let ()
        (let-values ([(st sz) (parser bv-info)])
          (if st
            (let ()
              (vector-set! bv-info 1
                (list->vector
                  (append (vector->list rst-v) `(,st))))
              (set-bv-cur-pos bv-info (+ cur-bv-pos sz))
              (set-bv-just-used bv-info sz)
              (set-bv-use-sz bv-info (- use-bv-sz sz))
              st)
            (error 'bin-parser-holder "no result!" st))))
      (error 'bin-parser-holder "bv error!" bv-info use-bv-sz))))

(define-syntax bin-eat
  (lambda (x)
    (syntax-case x ()
      [(k parser bv-info)
        (symbol? (syntax->datum #'name))
        #'(begin
            (bin-parser-holder parser bv-info))]
      [_ (syntax-error x "bin-eat invalid syntax!")])))

(define-syntax with-bin-sub-tree
  (lambda (x)
    (syntax-case x ()
      [(k bv-info e ...)
        #'(let ([rst-v (get-rst-v bv-info)])
            (set-rst-v bv-info (vector))
            (let ()
              e
              ...)
            (set-rst-v bv-info (list->vector
                                 (append (vector->list rst-v) (vector->list (vector (get-rst-v bv-info)))))))]
      [_ (syntax-error x "bin-eat-in-sub-tree invalid syntax!")])))

(define bin-subtree
  (lambda (f bv-info . param)
    (with-bin-sub-tree bv-info
      (apply f bv-info param))))

(define is-parse-to-end
  (lambda (bv-info)
    (>= (vector-ref (vector-ref bv-info 0) 1) (bytevector-length (vector-ref (vector-ref bv-info 0) 0)))))

(define parse-tcp
  (lambda (bv-info)
    (define the-tcp-hdr (bin-eat parse-tcp-hdr bv-info))
    (when (> (get-bv-use-sz bv-info) 0)
      (bin-eat parse-bytes bv-info))))

(define parse-ipv4
  (lambda (bv-info)
    (define the-ipv4-hdr (bin-eat parse-ipv4-hdr bv-info))
    (if (= #x06 (ipv4-hdr-proto the-ipv4-hdr))
      (bin-subtree parse-tcp bv-info)
      (bin-eat parse-bytes bv-info))))

(define parse-ethernet-ii
  (lambda (bv-info)
    ;; l2
    (define the-ethernet-ii-hdr (bin-eat parse-ethernet-ii-hdr bv-info))
    (if (= #x0800 (ethernet-ii-hdr-ethertype the-ethernet-ii-hdr))
      (bin-subtree parse-ipv4 bv-info)
      (bin-eat parse-bytes bv-info))
    ;; l2 padding
    (when (> (get-bv-use-sz bv-info) 0)
      (bin-eat parse-bytes bv-info))))

(define parse-pcaprec
  (lambda (bv-info file-endian)
    (define the-pcaprec-hdr (bin-eat parse-pcaprec-hdr bv-info))
    (set-bv-use-sz bv-info (big-endian-to file-endian (pcaprec-hdr-incl-len the-pcaprec-hdr) 4))
    (bin-subtree parse-ethernet-ii bv-info)
    (set-bv-use-sz bv-info (- (bytevector-length (get-bv bv-info)) (get-bv-cur-pos bv-info)))))

(define parse-pcap
  (lambda (bv-info)
    (define file-endian 'little)
    (define break #f)
    (define the-pcap-hdr (bin-eat parse-pcap-hdr bv-info))
    (cond
      [(= (pcap-hdr-magic the-pcap-hdr) #xA1B2C3D4) (set! file-endian 'big)]
      [(= (pcap-hdr-magic the-pcap-hdr) #xD4C3B2A1) (set! file-endian 'little)]
      [else
        (error 'parse-file "unsupported pcap file magic!" (pcap-hdr-magic the-pcap-hdr))])
    (do ()
      (break)
      (bin-subtree parse-pcaprec bv-info file-endian)
      (when (is-parse-to-end bv-info)
        (set! break #t)))))

(define make-bin-info
  (lambda (data-bv)
    (vector (vector data-bv 0 (bytevector-length data-bv) 0) (vector))))

(let ([pathname "C:\\Users\\Saigut\\Desktop\\pktpcap.pcap"]
       [fp 0]
       [header 0]
       [rst-v (vector)]
       [bv-info (vector)])
  (set! fp (open-file-input-port pathname (file-options no-create) 'block))
  (set! header (get-bytevector-all fp))

  (set! bv-info (make-bin-info header))
  (parse-pcap bv-info)
  (time (traverse-bin-struct (vector-ref bv-info 1)))

  (close-port fp))
