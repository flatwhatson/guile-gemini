(use-modules (guix packages)
             (guix build-system guile)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages tls)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define %source-dir
  (dirname (current-filename)))

(define %git-commit
  (get-line (open-input-pipe "git rev-parse HEAD")))

(define guile-gemini
  (package
    (name "guile-gemini")
    (version (git-version "0.1" "HEAD" %git-commit))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments '(#:not-compiled-file-regexp "guix\\.scm$"))
    (inputs `(("guile" ,guile-3.0-latest)
              ("gnutls" ,gnutls)
              ("guile-fibers" ,guile-fibers)))
    (home-page "https://github.com/flatwhatson/guile-gemini")
    (synopsis "Guile implementation of the Gemini protocol")
    (description
     "Guile Gemini is an implementation of the Gemini protocol in Guile Scheme,
providing both client and server functionality.  It uses GnuTLS to meet
Gemini's TLS requirements, and Guile Fibers for concurrency.")
    (license license:lgpl3+)))

guile-gemini
