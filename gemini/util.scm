(define-module (gemini util)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (bytevector-slice get-bytevector-crlf))

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

(define (get-bytevector-crlf port maxlen)
  (let ((bv (make-bytevector maxlen)))
    (let loop ((start 0))
      (let* ((count (get-bytevector-some! port bv start (- maxlen start)))
             (eof (eof-object? count))
             (end (and (not eof)
                       (bytevector-find-crlf bv start count))))
        (cond (eof #f)
              (end (unget-bytevector port bv (+ end 2))
                   (bytevector-slice bv 0 end))
              (else (loop (+ start count))))))))
