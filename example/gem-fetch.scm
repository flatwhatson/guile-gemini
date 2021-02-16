#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (dirname (dirname (current-filename))))
(use-modules (gemini client)
             (gemini request)
             (gemini response)
             (gnutls)
             (ice-9 getopt-long)
             (ice-9 regex)
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

(define (normalize-uri uri)
  (let* ((uri (if (string-match "^([a-zA-Z][a-zA-Z0-9.+-]*:)?//" uri) uri
                  (string-append "gemini://" uri)))
         (uri (string->uri-reference uri)))
    (build-uri (or (uri-scheme uri) 'gemini)
               #:userinfo (uri-userinfo uri)
               #:host (uri-host uri)
               #:port (uri-port uri)
               #:path (let ((path (uri-path uri)))
                        (cond ((not path) "/")
                              ((string-null? path) "/")
                              (else path)))
               #:query (uri-query uri)
               #:fragment (uri-fragment uri))))

(define (build-request uri)
  (build-gemini-request #:uri (normalize-uri uri)))

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
        (let-values (((req) (build-request uri))
                     ((host port) (parse-proxy proxy))
                     ((cred) (load-credentials cert pkey)))
          (let* ((rsp (send-gemini-request req host port cred))
                 (body (gemini-response-body rsp)))
            (when body
              (let* ((text (utf8->string body))
                     (last (string-ref text (1- (string-length text)))))
                (display text)
                (unless (char=? last #\newline)
                  (newline)))))))))
