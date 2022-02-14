(define-module (gemini util tls)
  #:use-module (gemini util log)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-11)
  #:export (tls-handshake))

;; (set-log-level! 10)
;; (set-log-procedure!
;;  (lambda (level str)
;;    (log-info "gnutls: [~a] ~a" level str)))

(define-syntax-rule (try body ...)
  (begin
    (catch 'gnutls-error
      (lambda ()
        body)
      (lambda err
        (log-error "~s" err)))
    ...))

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
               (apply throw key err proc rest))))))

  (try
   (log-debug "peer-certificate-status: ~s"
              (peer-certificate-status session))
   (log-debug "session-our-certificate-chain: ~s"
              (session-our-certificate-chain session))
   (log-debug "session-peer-certificate-chain: ~s"
              (session-peer-certificate-chain session))
   (log-debug "session-authentication-type: ~s"
              (session-authentication-type session))
   (log-debug "session-client-authentication-type: ~s"
              (session-client-authentication-type session))
   (log-debug "session-server-authentication-type: ~s"
              (session-server-authentication-type session)))

  (let* ((chain (session-peer-certificate-chain session))
         (cert (and (not (null? chain))
                    (import-x509-certificate (car chain) x509-certificate-format/der))))
    (when cert
      (try
       (log-debug "x509-certificate-dn: ~s"
                  (x509-certificate-dn cert))
       (log-debug "x509-certificate-dn-oid: ~s"
                  (let loop ((i 0) (res '()))
                    (let ((oid (x509-certificate-dn-oid cert i)))
                      (if (not oid)
                          (reverse! res)
                          (loop (1+ i) (cons oid res))))))
       (log-debug "x509-certificate-issuer-dn: ~s"
                  (x509-certificate-issuer-dn cert))
       (log-debug "x509-certificate-issuer-dn-oid: ~s"
                  (let loop ((i 0) (res '()))
                    (let ((oid (x509-certificate-issuer-dn-oid cert i)))
                      (if (not oid)
                          (reverse! res)
                          (loop (1+ i) (cons oid res))))))
       (log-debug "x509-certificate-signature-algorithm: ~s"
                  (x509-certificate-signature-algorithm cert))
       (log-debug "x509-certificate-version: ~s"
                  (x509-certificate-version cert))
       (log-debug "x509-certificate-key-id: ~s"
                  (x509-certificate-key-id cert))
       (log-debug "x509-certificate-authority-key-id: ~s"
                  (x509-certificate-authority-key-id cert))
       (log-debug "x509-certificate-subject-key-id: ~s"
                  (x509-certificate-subject-key-id cert))
       (log-debug "x509-certificate-subject-alternative-name: ~s"
                  (let loop ((i 0) (res '()))
                    (let-values (((type name) (x509-certificate-subject-alternative-name cert i)))
                      (if (not type)
                          (reverse! res)
                          (loop (1+ i) (cons (cons type name) res))))))
       (log-debug "x509-certificate-public-key-algorithm: ~s"
                  (let-values (((algo bits) (x509-certificate-public-key-algorithm cert)))
                    (cons algo bits)))
       (log-debug "x509-certificate-key-usage: ~s"
                  (x509-certificate-key-usage cert))))))
