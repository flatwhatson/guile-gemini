#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))
(use-modules (gemini request)
             (gemini response)
             (gemini server)
             (gemini util log)
             (fibers timers)
             (gnutls)
             (ice-9 getopt-long)
             (ice-9 textual-ports)
             (rnrs bytevectors)
             (srfi srfi-11)
             (web uri))

(define (print-help args)
  (display (string-append "\
usage: " (car args) " [options]

options:
  -h, --help                   Display this help
  -v, --verbose                Enable additional log messages
  -c, --cert=path/to/cert.pem  Server certificate file
  -k, --key=path/to/key.pem    Server private key file

Start a simple Gemini server.
")))

(define (handle-request req)
  (build-gemini-response
   #:status 20
   #:meta "text/gemini"
   #:body (lambda (port)
            (let loop ((n 10))
              (cond ((= n 0)
                     (format port "Blast off!\n"))
                    (else
                     (format port "~a...\n" n)
                     (force-output port)
                     (sleep 1)
                     (loop (1- n))))))))

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
                        (cert    (single-char #\c) (value #t))
                        (key     (single-char #\k) (value #t))))
         (opts    (getopt-long args option-spec))
         (help    (option-ref opts 'help #f))
         (verbose (option-ref opts 'verbose #f))
         (cert    (option-ref opts 'cert #f))
         (key     (option-ref opts 'key #f)))
    (cond (help
           (print-help args))
          (else
           (when verbose
             (set-gemini-log-level! 'debug))
           (let ((creds (load-credentials cert key)))
             (run-server handle-request #:credentials creds))))))
