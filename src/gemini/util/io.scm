(define-module (gemini util io)
  #:use-module (gnutls)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (bytevector-slice
            get-tls-bytevector-crlf
            get-tls-bytevector-eof
            get-tls-bytevector-some))

(define *cr-byte* (char->integer #\return))
(define *lf-byte* (char->integer #\newline))

(define (bytevector-find-crlf bv start count)
  "Return the index of CRLF in BV, or #f if not found."
  (let ((end (+ start count)))
    (let loop ((ix (max start 1)))
      (cond ((>= ix end)
             #f)
            ((and (= *cr-byte* (bytevector-u8-ref bv (1- ix)))
                  (= *lf-byte* (bytevector-u8-ref bv ix)))
             (1- ix))
            (else
             (loop (1+ ix)))))))

(define* (bytevector-slice bv start #:optional
                           (count (- (bytevector-length bv) start)))
  "Return a bytevector referencing COUNT bytes of BV from index START."
  (pointer->bytevector
   (bytevector->pointer bv) count start))

(define (handle-premature-termination thunk)
  (catch 'gnutls-error
    thunk
    (lambda (key err proc . rest)
      (if (eq? err error/premature-termination)
          (eof-object)
          (apply throw key err proc rest)))))

(define (get-tls-bytevector-some! port bv start count)
  "A wrapper around get-bytevector-some! which handles GnuTLS
E_PREMATURE_TERMINATION errors by returning an end-of-file object."
  (handle-premature-termination
    (lambda ()
      (get-bytevector-some! port bv start count))))

(define (get-tls-bytevector-some port)
  "A wrapper around get-bytevector-some which handles GnuTLS
E_PREMATURE_TERMINATION errors by returning an end-of-file object."
  (handle-premature-termination
    (lambda ()
      (get-bytevector-some port))))

(define (get-tls-bytevector-crlf port maxlen)
  "Read a CRLF-terminated sequence of up to MAXLEN bytes from PORT, and return
a bytevector containing the octects read up-to but not including the CRLF.  If
a CRLF is not found before MAXLEN or end-of-file, returns #f."
  (let ((bv (make-bytevector (+ maxlen 2))))
    (let loop ((start 0)
               (count (+ maxlen 2)))
      (let* ((res (get-tls-bytevector-some! port bv start count))
             (eof (eof-object? res))
             (end (and (not eof)
                       (bytevector-find-crlf bv start res))))
        (cond (eof
               (unget-bytevector port bv)
               #f)
              (end
               (unget-bytevector port bv
                                 (+ end 2)
                                 (- (+ start res) (+ end 2)))
               (bytevector-slice bv 0 end))
              (else
               (loop (+ start res) (- count res))))))))

(define (get-tls-bytevector-eof port)
  "Read from PORT until the end-of-file is reached, and return a bytevector
containing the octets read.  If no data is available, returns #f."
  (let loop ((bv (make-bytevector 4096))
             (start 0)
             (count 4096))
    (let ((res (get-tls-bytevector-some! port bv start count)))
      (cond ((eof-object? res)
             (if (zero? start) #f
                 (bytevector-slice bv 0 start)))
            ((< res count)
             (loop bv (+ start res) (- count res)))
            (else
             (let* ((len (bytevector-length bv))
                    (new (make-bytevector (* 2 len))))
               (bytevector-copy! bv 0 new 0 len)
               (loop new len len)))))))
