(define-module (gemini server)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (gemini request)
  #:use-module (gemini response)
  #:use-module (gnutls)
  #:use-module (ice-9 suspendable-ports)
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

    (define (wait-until-readable)
      ((current-read-waiter)
       (session-record-port session)))

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

    session))

(define (handle-client client cred handler)
  (let* ((session (open-session client cred))
         (record  (session-record-port session)))

    (setvbuf record 'block)
    (let* ((req (read-gemini-request record))
           (rsp (handler req)))

      (write-gemini-response rsp record)
      (close-port record)
      (close-port client))))

(define (accept-loop server cred handler)
  (let loop ()
    (let ((client (accept server (logior SOCK_NONBLOCK SOCK_CLOEXEC))))
      (spawn-fiber
       (lambda ()
         (handle-client (car client) cred handler))
       #:parallel? #t)
      (loop))))

(define* (run-server handler #:key
                     (host #f)
                     (family AF_INET)
                     (addr (if host (inet-pton family host) INADDR_LOOPBACK))
                     (port 1965)
                     (socket (make-default-socket family addr port))
                     cred)
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
