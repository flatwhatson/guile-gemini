(define-module (gemini response)
  #:use-module (gemini util io)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-41)
  #:export (gemini-response?
            gemini-response-status
            gemini-response-meta
            gemini-response-body
            gemini-response-body-bytes
            gemini-response-body-stream
            gemini-response-success?

            build-gemini-response
            read-gemini-response
            write-gemini-response))

;; 10 INPUT                       (prompt)
;; 11 SENSITIVE INPUT             (prompt)
;; 20 SUCCESS                     (mime type)
;; 30 REDIRECT - TEMPORARY        (new url)
;; 31 REDIRECT - PERMANENT        (new url)
;; 40 TEMPORARY FAILURE           (error message)
;; 41 SERVER UNAVAILABLE          (error message)
;; 42 CGI ERROR                   (error message)
;; 43 PROXY ERROR                 (error message)
;; 44 SLOW DOWN                   (error message)
;; 50 PERMANENT FAILURE           (error message)
;; 51 NOT FOUND                   (error message)
;; 52 GONE                        (error message)
;; 53 PROXY REQUEST REFUSED       (error message)
;; 59 BAD REQUEST                 (error message)
;; 60 CLIENT CERTIFICATE REQUIRED (error message)
;; 61 CERTIFICATE NOT AUTHORISED  (error message)
;; 62 CERTIFICATE NOT VALID       (error message)

(define-record-type <gemini-response>
  (make-gemini-response status meta body peer)
  gemini-response?
  (status gemini-response-status)
  (meta gemini-response-meta)
  (body gemini-response-body)
  (peer gemini-response-peer))

(define (gemini-status-success? code)
  (and (>= code 20) (< code 30)))

(define (gemini-response-success? rsp)
  (gemini-status-success?
   (gemini-response-status rsp)))

(define (gemini-response-body-bytes rsp)
  (let ((port (gemini-response-body rsp)))
    (and (port? port)
         (let ((bytes (get-tls-bytevector-eof port)))
           (close-port port)
           bytes))))

(define (gemini-response-body-stream rsp)
  (let ((port (gemini-response-body rsp)))
    (and (port? port)
         (stream-let loop ()
           (match (get-tls-bytevector-some port)
             ((? eof-object? _)
              (close-port port)
              stream-null)
             (chunk
              (stream-cons chunk (loop))))))))

(define (bad-response message . args)
  (throw 'bad-response message args))

(define (validate-gemini-response rsp)
  ;; TODO: check for valid status code
  ;; TODO: check for valid meta value
  #t)

(define* (build-gemini-response #:key status meta body peer (validate? #t))
  "Construct a Gemini response."
  (let ((rsp (make-gemini-response status meta body peer)))
    (when validate?
      (validate-gemini-response rsp))
    rsp))

(define *space-byte* (char->integer #\space))
(define *zero-byte*  (char->integer #\0))
(define *one-byte*   (char->integer #\1))
(define *six-byte*   (char->integer #\6))
(define *nine-byte*  (char->integer #\9))

(define (valid-status-bytes? b1 b2)
  (and (>= b1 *one-byte*) (<= b1 *six-byte*)
       (>= b2 *zero-byte*) (<= b2 *nine-byte*)))

(define (parse-status-bytes b1 b2)
  (+ (* (- b1 *zero-byte*) 10)
     (- b2 *zero-byte*)))

(define* (read-gemini-response port #:optional peer)
  "Read a Gemini response from PORT with optional PEER information."
  (let* ((data (or (get-tls-bytevector-crlf port (+ 1024 3))
                   (bad-response "Invalid response")))
         (b1 (bytevector-u8-ref data 0))
         (b2 (bytevector-u8-ref data 1))
         (b3 (bytevector-u8-ref data 2))
         (rest (bytevector-slice data 3)))
    (unless (= b3 *space-byte*)
      (bad-response "Missing status separator"))
    (unless (valid-status-bytes? b1 b2)
      (bad-response "Invalid status code"))
    (let* ((status (parse-status-bytes b1 b2))
           (meta (utf8->string rest))
           (body (and (gemini-status-success? status) port))
           (rsp (make-gemini-response status meta body peer)))
      (validate-gemini-response rsp)
      rsp)))

(define (write-gemini-response rsp port)
  "Write the given Gemini response to PORT."
  (put-string port (number->string (gemini-response-status rsp)))
  (put-char port #\space)
  (put-string port (gemini-response-meta rsp))
  (put-string port "\r\n")
  (match (gemini-response-body rsp)
    ((? bytevector? body)
     (put-bytevector port body))
    ((? string? body)
     (put-bytevector port (string->utf8 body)))
    ((? procedure? callback)
     (callback port))))
