#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (dirname (dirname (current-filename))))
(use-modules (gemini client)
             (gemini request)
             (gemini response)
             (ice-9 getopt-long)
             (rnrs bytevectors)
             (web uri))

(define (print-help args)
  (display (string-append "\
usage: " (car args) " [options] URI

options:
  -h, --help       Display this help
  -a, --addr       Host to connect to (default from URI)
  -p, --port       Port to connect to (default from URI)

Send a Gemini request and print the response.
")))

(define (main args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                        (addr (single-char #\a) (value #t))
                        (port (single-char #\p) (value #t))))
         (opts (getopt-long args option-spec))
         (help (option-ref opts 'help #f))
         (addr (option-ref opts 'addr #f))
         (port (option-ref opts 'port #f))
         (rest (option-ref opts '() '()))
         (uri (and (pair? rest) (car rest))))
    (if (or help (not uri))
        (print-help args)
        (let ((req (build-gemini-request #:uri (string->uri uri))))
          (format #t "Request: ~a\n"
                  (uri->string (gemini-request-uri req)))
          (let ((rsp (send-gemini-request req addr port)))
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
