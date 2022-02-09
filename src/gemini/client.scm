(define-module (gemini client)
  #:use-module (gemini request)
  #:use-module (gemini response)
  #:use-module (gemini util log)
  #:use-module (gemini util tls)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:export (send-gemini-request))

(define (resolve-addresses host port)
  (delete-duplicates
   (getaddrinfo host (if port (number->string port) "1965") AI_NUMERICSERV)
   (lambda (ai1 ai2)
     (equal? (addrinfo:addr ai1) (addrinfo:addr ai2)))))

(define (open-socket host port)
  (let loop ((addresses (resolve-addresses host port)))
    (let* ((addr (addrinfo:addr (car addresses)))
           (sock (with-fluids ((%default-port-encoding #f))
                   (socket (sockaddr:fam addr) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (log-info "Connecting to ~a:~a [~a]"
                    host (sockaddr:port addr)
                    (inet-ntop (sockaddr:fam addr)
                               (sockaddr:addr addr)))
          (connect sock addr)
          (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
          sock)
        (lambda (key subr msg args rest)
          (log-warn "~a [~a]" (apply format #f msg args) (car rest))
          (close sock)
          (if (null? (cdr addresses))
              (throw key subr msg args rest)
              (loop (cdr addresses))))))))

(define (open-session socket host cred)
  (let ((session (make-session connection-end/client)))
    (set-session-server-name! session server-name-type/dns host)
    (set-session-transport-fd! session (fileno socket))
    (set-session-default-priority! session)
    (set-session-credentials! session cred)

    ;; TODO: require TLS 1.3 when sending client cert
    ;; TODO: try regular CA verification of the server cert
    ;; TODO: fall back to TOFU for self-signed certificates
    ;; https://drewdevault.com/2020/09/21/Gemini-TOFU.html

    (log-info "Performing handshake")
    (tls-handshake session)

    (let* ((chain (session-peer-certificate-chain session))
           (cert (import-x509-certificate (car chain) x509-certificate-format/der)))
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

    session))

(define* (send-gemini-request request #:optional host port credentials)
  "Send a Gemini REQUEST and return the response.

By default a new TLS connection will be established with the host and port
described in the request URI, with no TLS client certificate.

The optional HOST and PORT arguments can be used to specify an alternative
server address, useful for sending proxied requests or bypassing DNS lookup.

The optional CREDENTIALS argument can be used to provide client credentials
for the TLS negotiation."
  (let* ((host (or host (uri-host (gemini-request-uri request))))
         (port (or port (uri-port (gemini-request-uri request))))
         (credentials (or credentials (make-certificate-credentials)))

         (socket  (open-socket host port))
         (session (open-session socket host credentials))
         (record  (session-record-port session)))

    (log-info "Sending request: ~a"
              (uri->string (gemini-request-uri request)))
    (setvbuf record 'block)
    (write-gemini-request request record)
    (force-output record)

    (let ((response (read-gemini-response record)))
      (log-info "Received response: ~a ~a"
                (gemini-response-status response)
                (gemini-response-meta response))
      response)))
