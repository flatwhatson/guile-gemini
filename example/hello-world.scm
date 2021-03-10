#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (dirname (dirname (current-filename))))
(use-modules (gemini request)
             (gemini response)
             (gemini server)
             (gemini util log)
             (gnutls)
             (ice-9 getopt-long)
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
  -k, --pkey=path/to/pkey.pem  Server private key file

Start a simple Gemini server.
")))

(define (handle-request req)
  (build-gemini-response
   #:status 20
   #:meta "text/gemini"
   #:body (string->utf8 "Hello, world!")))

(define (load-credentials cert pkey)
  (let ((cred (make-certificate-credentials)))
    (when (and cert pkey)
      (set-certificate-credentials-x509-key-files!
       cred cert pkey x509-certificate-format/pem))
    cred))

(define (main args)
  (let* ((option-spec '((help    (single-char #\h) (value #f))
                        (verbose (single-char #\v) (value #f))
                        (cert    (single-char #\c) (value #t))
                        (pkey    (single-char #\k) (value #t))))
         (opts    (getopt-long args option-spec))
         (help    (option-ref opts 'help #f))
         (verbose (option-ref opts 'verbose #f))
         (cert    (option-ref opts 'cert #f))
         (pkey    (option-ref opts 'pkey #f)))
    (cond (help
           (print-help args))
          (else
           (when verbose
             (set-gemini-log-level! 'debug))
           (let ((cred (load-credentials cert pkey)))
             (run-server handle-request #:cred cred))))))
