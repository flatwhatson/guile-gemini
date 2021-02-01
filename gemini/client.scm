(define-module (gemini client)
  #:use-module (gemini request)
  #:use-module (gemini response)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:export (send-gemini-request))

;; (set-log-level! 10)
;; (set-log-procedure!
;;  (lambda (level str)
;;    (format #t "gnutls: [~a] ~a" level str)))

(define (resolve-addresses host port)
  (delete-duplicates
   (getaddrinfo host (if port (number->string port) "1965") AI_NUMERICSERV)
   (lambda (ai1 ai2)
     (equal? (addrinfo:addr ai1) (addrinfo:addr ai2)))))

(define (open-socket host port)
  (let loop ((addresses (resolve-addresses host port)))
    (let* ((ai   (car addresses))
           (sock (with-fluids ((%default-port-encoding #f))
                   (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (connect sock (addrinfo:addr ai))
          (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
          sock)
        (lambda args
          (close sock)
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

(define (open-session socket host cred)
  (let ((session (make-session connection-end/client)))
    (set-session-server-name! session server-name-type/dns host)
    (set-session-transport-fd! session (fileno socket))
    (set-session-default-priority! session)
    (set-session-credentials! session cred)

    (define wait-until-readable
      (let ((port (session-record-port session)))
        (lambda ()
          ((current-read-waiter) port))))

    (let continue-handshake ()
      (catch 'gnutls-error
        (lambda ()
          (handshake session))
        (lambda (key err proc . rest)
          (cond ((or (eq? err error/again)
                     (eq? err error/interrupted))
                 (wait-until-readable)
                 (continue-handshake))
                (else
                 (apply throw key err proc rest))))))

    (let* ((data (car (session-peer-certificate-chain session)))
           (cert (import-x509-certificate data x509-certificate-format/der)))
      (unless (x509-certificate-matches-hostname? cert host)
        (throw 'tls-certificate-error
               'host-mismatch host (x509-certificate-dn cert))))

    (let* ((status (peer-certificate-status session))
           (invalid (lset-intersection
                     eq? status (list certificate-status/expired
                                      certificate-status/not-activated))))
      (unless (null? invalid)
        (throw 'tls-certificate-error
               'invalid-certificate host invalid)))

    ;; TODO: server certificate fingerprint verification
    ;; https://drewdevault.com/2020/09/21/Gemini-TOFU.html

    session))

(define* (send-gemini-request req #:optional host port cred)
  (let* ((host (or host (uri-host (gemini-request-uri req))))
         (port (or port (uri-port (gemini-request-uri req))))
         (cred (or cred (make-certificate-credentials)))

         (socket  (open-socket host port))
         (session (open-session socket host cred))
         (record  (session-record-port session)))

    (setvbuf record 'block)
    (write-gemini-request req record)
    (force-output record)

    (let ((rsp (read-gemini-response record)))
      (close-port record)
      (close-port socket)
      rsp)))
