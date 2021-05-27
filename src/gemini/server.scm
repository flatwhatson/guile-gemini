(define-module (gemini server)
  #:use-module (fibers)
  #:use-module (fibers conditions)
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

(define (call-with-sigint thunk cvar)
  (let ((handler #f))
    (dynamic-wind
      (lambda ()
        (set! handler
          (sigaction SIGINT (lambda (sig)
                              (signal-condition! cvar)))))
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
         (record  (session-record-port session)))

    (setvbuf record 'block)
    (let ((req (read-gemini-request record)))
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
                     (host #f)
                     (family AF_INET)
                     (addr (if host (inet-pton family host) INADDR_LOOPBACK))
                     (port 1965)
                     (socket (make-default-socket family addr port))
                     cred)
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
                         (accept-loop socket cred handler)))
          (wait finished?))))
     finished?)))
