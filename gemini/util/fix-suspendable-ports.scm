(define-module (gemini util fix-suspendable-ports)
  #:use-module (ice-9 ports internal)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (rnrs bytevectors))

;; HACK: This module monkey-patches the `get-bytevector-some!' method from
;; (ice-9 suspenable-ports) to fix a bug which was introduced in guile commit
;; 8150823fc.  This should be removed once a fix has been merged in Guile.

(define fill-input
  (@@ (ice-9 suspendable-ports) fill-input))

(define (get-bytevector-some! port bv start count)
  (if (zero? count)
      0
      (call-with-values (lambda () (fill-input port 1 'binary))
        (lambda (buf cur buffered)
          (if (zero? buffered)
              (begin
                (set-port-buffer-has-eof?! buf #f)
                the-eof-object)
              (let ((transfer-size (min count buffered)))
                (bytevector-copy! (port-buffer-bytevector buf) cur
                                  bv start transfer-size)
                (set-port-buffer-cur! buf (+ cur transfer-size))
                transfer-size))))))

(module-set! (resolve-module '(ice-9 suspendable-ports))
             'get-bytevector-some! get-bytevector-some!)
