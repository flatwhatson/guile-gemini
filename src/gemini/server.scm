(define-module (gemini server)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (gemini peer)
  #:use-module (gemini request)
  #:use-module (gemini response)
  #:use-module (gemini util log)
  #:use-module (gemini util tls)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (web uri)
  #:export (run-server))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

(define (call-with-sigint thunk action)
  (let ((handler #f))
    (dynamic-wind
      (lambda ()
        (set! handler
              (sigaction SIGINT (lambda (sig)
                                  (action)))))
      thunk
      (lambda ()
        (if handler
            (sigaction SIGINT (car handler) (cdr handler))
            (sigaction SIGINT #f))))))

(define (open-session socket cred)
  (let ((session (make-session connection-end/server)))
    (set-session-transport-fd! session (fileno socket))
    (set-session-default-priority! session)
    (set-session-credentials! session cred)
    (set-server-session-certificate-request! session certificate-request/request)

    (log-info "Performing handshake")
    (tls-handshake session)

    session))

(define (handle-client client addr cred handler)
  (log-info "Connection from ~a:~a"
            (inet-ntop (sockaddr:fam addr)
                       (sockaddr:addr addr))
            (sockaddr:port addr))

  (let* ((session (open-session client cred))
         (record  (session-record-port session))
         (peer    (build-gemini-peer client session)))

    (when (gemini-peer-certificate peer)
      (log-info "Client common name: ~a" (gemini-peer-common-name peer))
      (log-info "Client fingerprint: ~a" (gemini-peer-fingerprint peer)))

    (setvbuf record 'block)
    (let ((req (read-gemini-request record peer)))
      (log-info "Received request: ~a"
                (uri->string (gemini-request-uri req)))

      (let ((rsp (handler req)))
        (log-info "Sending response: ~a ~a"
                  (gemini-response-status rsp)
                  (gemini-response-meta rsp))
        (write-gemini-response rsp record)
        (close-port record)
        (close-port client)))))

(define (accept-loop server cred handler)
  (let loop ()
    (let* ((result (accept server (logior SOCK_NONBLOCK SOCK_CLOEXEC)))
           (client (car result))
           (addr   (cdr result)))
      (spawn-fiber
       (lambda ()
         (define error #f)
         (catch #t
           (lambda ()
             (handle-client client addr cred handler))
           (lambda e
             (set! error e)))
         (close-port client)
         (when error
           (apply throw error)))
       #:parallel? #t)
      (loop))))

(define* (run-server handler #:key
                     host port
                     (family AF_INET)
                     (address (if host (inet-pton family host) INADDR_LOOPBACK))
                     (socket (make-default-socket family address (or port 1965)))
                     credentials)
  "Run a multi-threaded Gemini server.

HANDLER should be a procedure that takes a Gemini request and returns a Gemini
response.  CREDENTIALS should be a GnuTLS certificate credentials object
loaded with the server's TLS certificate and private key.

For example, here is a simple (*cough*) \"Hello, World!\" server:

@example
 (use-modules (gemini response)
              (gemini server)
              (gnutls))

 (define (handler request)
   (build-gemini-response
    #:status 20
    #:meta \"text/gemini\"
    #:body (string->utf8 \"Hello, world!\")))

 (define %server-cert \"path/to/server-cert.pem\")
 (define %server-key \"path/to/server-key.pem\")

 (define %server-creds
   (let ((creds (make-certificate-credentials)))
     (set-certificate-credentials-x509-key-files!
      creds %server-cert %server-key
      x509-certificate-format/pem)
     creds))

 (run-server handler #:credentials %server-creds)
@end example

By default the server will listen on localhost, port 1965.  These can be
overridden with the HOST and PORT keyword arguments.

The server uses fibers to handle multiple requests concurrently, so care
should be taken to ensure that any shared state accesses in HANDLER are
thread-safe."
  (let ((addr (getsockname socket)))
    (log-info "Listening on ~a:~a"
              (inet-ntop (sockaddr:fam addr)
                         (sockaddr:addr addr))
              (sockaddr:port addr)))
  (listen socket 1024)
  (sigaction SIGPIPE SIG_IGN)
  (let ((finished? (make-condition)))
    (call-with-sigint
     (lambda ()
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda ()
                         (accept-loop socket credentials handler)))
          (wait finished?))))
     (lambda ()
       (signal-condition! finished?)))))
