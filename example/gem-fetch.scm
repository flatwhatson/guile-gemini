#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (dirname (dirname (current-filename))))
(use-modules (gemini client)
             (gemini request)
             (gemini response)
             (gnutls)
             (ice-9 getopt-long)
             (rnrs bytevectors)
             (srfi srfi-11)
             (web uri))

(define (print-help args)
  (display (string-append "\
usage: " (car args) " [options] URI

options:
  -h, --help                   Display this help
  -p, --proxy=HOST:PORT        Proxy via HOST:PORT
  -c, --cert=path/to/cert.pem  Client certificate file
  -k, --pkey=path/to/pkey.pem  Client private key file

Send a Gemini request and print the response.
")))

(define (parse-proxy proxy)
  (cond ((not proxy)
         (values #f #f))
        ((string-contains proxy ":")
         (apply values (string-split proxy #\:)))
        (else
         (values proxy #f))))

(define (load-credentials cert pkey)
  (let ((cred (make-certificate-credentials)))
    (when (and cert pkey)
      (set-certificate-credentials-x509-key-files!
       cred cert pkey x509-certificate-format/pem))
    cred))

(define (main args)
  (let* ((option-spec '((help  (single-char #\h) (value #f))
                        (proxy (single-char #\p) (value #t))
                        (cert  (single-char #\c) (value #t))
                        (pkey  (single-char #\k) (value #t))))
         (opts  (getopt-long args option-spec))
         (help  (option-ref opts 'help #f))
         (proxy (option-ref opts 'proxy #f))
         (cert  (option-ref opts 'cert #f))
         (pkey  (option-ref opts 'pkey #f))
         (rest  (option-ref opts '() '()))
         (uri   (and (pair? rest) (car rest))))
    (if (or help (not uri))
        (print-help args)
        (let-values (((req) (build-gemini-request #:uri (string->uri uri)))
                     ((host port) (parse-proxy proxy))
                     ((cred) (load-credentials cert pkey)))
          (format #t "Request: ~a\n"
                  (uri->string (gemini-request-uri req)))
          (let ((rsp (send-gemini-request req host port cred)))
            (format #t "Response: ~a ~a\n"
                    (gemini-response-status rsp)
                    (gemini-response-meta rsp))
            (let ((body (gemini-response-body rsp)))
              (when body
                (let* ((text (utf8->string body))
                       (last (string-ref text (1- (string-length text)))))
                  (display text)
                  (unless (char=? last #\newline)
                    (newline))))))))))
