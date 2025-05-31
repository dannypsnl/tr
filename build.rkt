#lang racket
(require racket/system)

(define (build-embed addr)
  (define output-dir (build-path "output/" addr))
  (make-directory* output-dir)

  (define f (open-output-file
    (build-path output-dir "embed.html")
    #:exists 'replace))

  (process*/ports f (current-input-port) (current-output-port) (find-executable-path "racket") (path-add-extension addr ".scrbl"))

  (printf "embed ~s\n" addr))

(define (build-index addr)
  (define output-dir (build-path "output/" addr))
  (build-path output-dir "embed.html")

  (define f (open-output-file
    (build-path output-dir "index.html")
    #:exists 'replace))
  (process*/ports f (current-input-port) (current-output-port) (find-executable-path "racket") (path-add-extension addr ".scrbl"))

  (printf "index ~s\n" addr)
  )

(define addr-list
 '("xxx-0001"
    "xxx-0002"))

(for ([addr addr-list])
  (build-embed addr))
(for ([addr addr-list])
  (build-index addr))
