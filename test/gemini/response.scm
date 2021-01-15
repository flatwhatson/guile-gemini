(use-modules (srfi srfi-64)
             (gemini response))

(test-begin "gemini-response")

(test-group "build-gemini-response"
  (let ((rsp (build-gemini-response #:status 20
                                    #:meta "text/gemini; charset=utf-8")))
    (test-eq 20 (gemini-response-status rsp))
    (test-equal "text/gemini; charset=utf-8" (gemini-response-meta rsp))))

(test-group "read-gemini-response"
  (let* ((data "20 text/gemini; charset=utf-8\r\n")
         (rsp (call-with-input-string data read-gemini-response)))
    (test-eq 20 (gemini-response-status rsp))
    (test-equal "text/gemini; charset=utf-8" (gemini-response-meta rsp))))

(test-group "write-gemini-request"
  (let* ((rsp (build-gemini-response #:status 20
                                     #:meta "text/gemini; charset=utf-8"))
         (data (call-with-output-string
                 (lambda (port)
                   (write-gemini-response rsp port)))))
    (test-equal "20 text/gemini; charset=utf-8\r\n" data)))

(test-end)
