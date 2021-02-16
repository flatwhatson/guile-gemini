(define-module (gemini util log)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (log-debug
            log-info
            log-warn
            log-error))

(define log-port current-error-port)

(define log-level)
(define log-levels-enabled)
(define log-levels '(debug info warn error))

(define (set-log-level! level)
  (set! log-level level)
  (set! log-levels-enabled
    (let-values (((off on) (break (cut eq? level <>) log-levels)))
      on)))

(set-log-level! 'debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-log-msg level msg . args)
  (let ((time (gettimeofday)))
    (apply format (log-port)
           (string-append "[~d.~6,'0d] ~a: " msg "\n")
           (car time) (cdr time) level args)))

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
