(define-module (gemini util log)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:export (set-gemini-log-level!
            set-gemini-log-port!
            log-debug
            log-info
            log-warn
            log-error))

(define log-port)
(define log-level)
(define log-levels-enabled)
(define log-levels '(debug info warn error))

(define (set-gemini-log-level! level)
  (set! log-level level)
  (set! log-levels-enabled
    (let-values (((off on) (break (cut eq? level <>) log-levels)))
      on)))

(define (set-gemini-log-port! port-fn)
  (set! log-port port-fn))

(set-gemini-log-level! 'info)
(set-gemini-log-port! current-error-port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-log-msg level msg . args)
  (let* ((time (gettimeofday))
         (line (apply format #f
                      (string-append "[~d.~6,'0d] ~a: " msg "\n")
                      (car time) (cdr time) level args)))
    (monitor
     (put-string (log-port) line))))

(define-syntax-rule (log-msg level msg args ...)
  (when (memq level log-levels-enabled)
    (write-log-msg level msg args ...)))

(define-syntax-rule (log-debug msg args ...)
  (log-msg 'debug msg args ...))

(define-syntax-rule (log-info msg args ...)
  (log-msg 'info msg args ...))

(define-syntax-rule (log-warn msg args ...)
  (log-msg 'warn msg args ...))

(define-syntax-rule (log-error msg args ...)
  (log-msg 'error msg args ...))
