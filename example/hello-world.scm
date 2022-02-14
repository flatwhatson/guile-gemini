#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))
(use-modules (gemini request)
             (gemini response)
             (gemini server)
             (gemini util log)
             (gnutls)
             (ice-9 getopt-long)
             (rnrs bytevectors)
             (srfi srfi-11)
             (srfi srfi-41)
             (web uri))

(define (print-help args)
  (display (string-append "\
usage: " (car args) " [options]

options:
  -h, --help                   Display this help
  -v, --verbose                Enable additional log messages
  -l, --listen=HOST:PORT       Listen on HOST:PORT (default localhost:1965)
  -c, --cert=path/to/cert.pem  Server certificate file
  -k, --key=path/to/key.pem    Server private key file

Start a simple Gemini server.
")))

(define (handle-request req)
  (build-gemini-response
   #:status 20
   #:meta "text/gemini"
   #:body "Hello, world!"))

(define (parse-address address)
  (cond ((not address)
         (values #f #f))
        ((string-contains address ":")
         (apply values (string-split address #\:)))
        (else
         (values address #f))))

(define (load-credentials cert key)
  (let ((creds (make-certificate-credentials)))
    (when (and cert key)
      (log-debug "Loading cert: ~a" cert)
      (log-debug "Loading key: ~a" key)
      (set-certificate-credentials-x509-key-files!
       creds cert key x509-certificate-format/pem))
    creds))

(define (main args)
  (let* ((option-spec '((help    (single-char #\h) (value #f))
                        (verbose (single-char #\v) (value #f))
                        (listen  (single-char #\l) (value #t))
                        (cert    (single-char #\c) (value #t))
                        (key     (single-char #\k) (value #t))))
         (opts    (getopt-long args option-spec))
         (help    (option-ref opts 'help #f))
         (verbose (option-ref opts 'verbose #f))
         (listen  (option-ref opts 'listen #f))
         (cert    (option-ref opts 'cert #f))
         (key     (option-ref opts 'key #f)))
    (cond (help
           (print-help args))
          (else
           (when verbose
             (set-gemini-log-level! 'debug))
           (let-values (((host port) (parse-address listen))
                        ((creds) (load-credentials cert key)))
             (run-server handle-request
                         #:host host
                         #:port port
                         #:credentials creds))))))
