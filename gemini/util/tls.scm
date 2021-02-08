(define-module (gemini util tls)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
  #:export (tls-handshake))

(define (wait-for-readable session)
  "Wait until the record port for SESSION is readable."
  ((current-read-waiter) (session-record-port session)))

(define (tls-handshake session)
  "Perform a TLS handshake on SESSION with transparent support for
non-blocking sockets.  Handles GnuTLS E_AGAIN and E_INTERRUPTED errors by
waiting until the record port is readable before continuing the handshake."
  (let continue-handshake ()
    (catch 'gnutls-error
      (lambda ()
        (handshake session))
      (lambda (key err proc . rest)
        (cond ((or (eq? err error/again)
                   (eq? err error/interrupted))
               (wait-for-readable session)
               (continue-handshake))
              (else
               (apply throw key err proc rest)))))))
