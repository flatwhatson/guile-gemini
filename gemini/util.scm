(define-module (gemini util)
  #:use-module (gnutls)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (bytevector-slice
            get-bytevector-crlf
            get-bytevector-eof))

(define *cr-byte* (char->integer #\return))
(define *lf-byte* (char->integer #\newline))

(define (bytevector-find-crlf bv start count)
  (let ((end (+ start count)))
    (let loop ((ix (max start 1)))
      (cond ((>= ix end)
             #f)
            ((and (= *cr-byte* (bytevector-u8-ref bv (1- ix)))
                  (= *lf-byte* (bytevector-u8-ref bv ix)))
             (1- ix))
            (else
             (loop (1+ ix)))))))

(define* (bytevector-slice bv start #:optional (count
                                                (- (bytevector-length bv)
                                                   start)))
  (pointer->bytevector
   (bytevector->pointer bv) count start))

(define (get-bytevector-safe! port bv start count)
  (catch 'gnutls-error
    (lambda ()
      (get-bytevector-some! port bv start count))
    (lambda (key err proc . rest)
      (if (eq? err error/premature-termination)
          (eof-object)
          (apply throw key err proc rest)))))

(define (get-bytevector-crlf port maxlen)
  (let ((bv (make-bytevector (+ maxlen 2))))
    (let loop ((start 0)
               (count (+ maxlen 2)))
      (let* ((res (get-bytevector-safe! port bv start count))
             (eof (eof-object? res))
             (end (and (not eof)
                       (bytevector-find-crlf bv start res))))
        (cond (eof #f)
              (end (unget-bytevector port bv
                                     (+ end 2)
                                     (- (+ start res)
                                        (+ end 2)))
                   (bytevector-slice bv 0 end))
              (else (loop (+ start res) (- count res))))))))

(define (get-bytevector-eof port)
  (let loop ((bv (make-bytevector 4096))
             (start 0)
             (count 4096))
    (let ((res (get-bytevector-safe! port bv start count)))
      (cond ((eof-object? res)
             (bytevector-slice bv 0 start))
            ((< res count)
             (loop bv (+ start res) (- count res)))
            (else
             (let* ((len (bytevector-length bv))
                    (new (make-bytevector (* 2 len))))
               (bytevector-copy! bv 0 new 0 len)
               (loop new len len)))))))
