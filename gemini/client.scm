(define-module (gemini client)
  #:use-module (gemini request)
  #:use-module (gemini response)
  #:use-module (gnutls)
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
    (let* ((ai   (car addresses))
           (sock (with-fluids ((%default-port-encoding #f))
                   (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (connect sock (addrinfo:addr ai))
          sock)
        (lambda args
          (close sock)
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

(define (open-session host socket)
  (let ((session (make-session connection-end/client)))
    (set-session-server-name! session server-name-type/dns host)
    (set-session-transport-fd! session (fileno socket))
    (set-session-default-priority! session)
    (set-session-credentials! session (make-certificate-credentials))

    (catch 'gnutls-error
      (lambda ()
        (handshake session))
      (lambda (key err proc . rest)
        (cond ((eq? err error/warning-alert-received)
               (format (current-error-port)
                       "warning: TLS warning alert received: ~a~%"
                       (alert-description->string (alert-get session)))
               (handshake session))
              (else
               (apply throw key err proc rest)))))

    session))

(define* (send-gemini-request req #:optional host port)
  (let* ((host (or host (uri-host (gemini-request-uri req))))
         (port (or port (uri-port (gemini-request-uri req))))
         (socket (open-socket host port))
         (session (open-session host socket))
         (record (session-record-port session)))
    (setvbuf record 'block)
    (write-gemini-request req record)
    (force-output record)
    (let ((rsp (read-gemini-response record)))
      (close-port record)
      (close-port socket)
      rsp)))
