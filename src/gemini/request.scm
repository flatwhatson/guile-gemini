(define-module (gemini request)
  #:use-module (gemini util io)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:export (gemini-request?
            gemini-request-uri
            gemini-request-peer

            build-gemini-request
            read-gemini-request
            write-gemini-request))

;; TODO: store peer address info in request
;; TODO: store peer certificate info in request

(define-record-type <gemini-request>
  (make-gemini-request uri peer)
  gemini-request?
  (uri gemini-request-uri)
  (peer gemini-request-peer))

(define (bad-request message . args)
  (throw 'bad-request message args))

(define (validate-gemini-request req)
  ;; TODO: maximum length 1024 bytes
  ;; TODO: userinfo is not allowed
  ;; TODO: host is required
  #t)

(define* (build-gemini-request #:key (scheme 'gemini)
                               host port (path "") query fragment
                               (validate? #t)
                               (uri (build-uri scheme
                                               #:host host #:port port
                                               #:path path #:query query
                                               #:fragment fragment
                                               #:validate? validate?))
                               peer)
  "Construct a Gemini request."
  (let ((req (make-gemini-request uri peer)))
    (when validate?
      (validate-gemini-request req))
    req))

(define* (read-gemini-request port #:optional peer)
  "Read a Gemini request from PORT with optional PEER information."
  (let* ((data (or (get-tls-bytevector-crlf port 1024)
                   (bad-request "Invalid request")))
         (uri (string->uri (utf8->string data)))
         (req (make-gemini-request uri peer)))
    (validate-gemini-request req)
    req))

(define (write-gemini-request req port)
  "Write the given Gemini request to PORT."
  (put-string port (uri->string (gemini-request-uri req)))
  (put-string port "\r\n"))
