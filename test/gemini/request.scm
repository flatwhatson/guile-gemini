(use-modules (srfi srfi-64)
             (web uri)
             (gemini request))

(test-begin "gemini-request")

(test-group "build-gemini-request"
  (let* ((req (build-gemini-request #:host "localhost"))
         (uri (gemini-request-uri req)))
    (test-assert (gemini-request? req))
    (test-equal "gemini://localhost" (uri->string uri))
    (test-eq    'gemini              (uri-scheme uri))
    (test-equal "localhost"          (uri-host uri))
    (test-eq    #f                   (uri-port uri))
    (test-equal ""                   (uri-path uri))
    (test-eq    #f                   (uri-query uri))
    (test-eq    #f                   (uri-fragment uri))))

(test-group "read-gemini-request"
  (let* ((data "gemini://localhost\r\n")
         (req (call-with-input-string data read-gemini-request))
         (uri (gemini-request-uri req)))
    (test-equal "gemini://localhost" (uri->string uri))))

(test-group "write-gemini-request"
  (let* ((req (build-gemini-request #:host "localhost"))
         (data (call-with-output-string
                 (lambda (port)
                   (write-gemini-request req port)))))
    (test-equal "gemini://localhost\r\n" data)))

(test-end)
