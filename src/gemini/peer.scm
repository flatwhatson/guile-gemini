(define-module (gemini peer)
  #:use-module (ice-9 regex)
  #:use-module (gnutls)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:export (gemini-peer?
            gemini-peer-sockaddr
            gemini-peer-host
            gemini-peer-port
            gemini-peer-certificate
            gemini-peer-common-name
            gemini-peer-fingerprint
            build-gemini-peer))

(define-record-type <gemini-peer>
  (make-gemini-peer sockaddr certificate)
  gemini-peer?
  (sockaddr gemini-peer-sockaddr)
  (certificate gemini-peer-certificate))

(define (gemini-peer-host peer)
  (let ((sa (gemini-peer-sockaddr peer)))
    (inet-ntop (sockaddr:fam sa) (sockaddr:addr sa))))

(define (gemini-peer-port peer)
  (let ((sa (gemini-peer-sockaddr peer)))
    (sockaddr:port sa)))

(define (gemini-peer-common-name peer)
  (and-let* ((crt (gemini-peer-certificate peer))
             (dn (x509-certificate-dn crt))
             (m (string-match "CN=([^,]+)" dn)))
    (match:substring m 1)))

(define (gemini-peer-fingerprint peer)
  (and-let* ((crt (gemini-peer-certificate peer))
             (key (x509-certificate-key-id crt)))
    (apply string-append
           (map (lambda (n)
                  (string-pad (number->string n 16) 2 #\0))
                (bytevector->u8-list key)))))

(define (build-gemini-peer socket session)
  (let* ((saddr (getsockname socket))
         (chain (session-peer-certificate-chain session))
         (data (and (not (null? chain)) (car chain)))
         (cert (and data (import-x509-certificate data x509-certificate-format/der))))
    (make-gemini-peer saddr cert)))
