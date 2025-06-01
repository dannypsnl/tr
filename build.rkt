#lang racket
(require racket/system
        dirname)

(define embed-header "#lang scribble/text
@(require \"tr.rkt\")
")

(define index-header "#lang scribble/text
@(require \"tr.rkt\")
@(generate-index #t)
")


(struct card (addr path) #:transparent)
(struct final-card (addr path target-path) #:transparent)

(define (compute-addr path)
  (basename (path-replace-extension path "")))

(define (produce-scrbl card-list mode)
  (for/list ([c card-list])
    (define tmp-path (build-path "_tmp" (string-append (card-addr c) "." mode ".scrbl")))
    (define f (open-output-file #:exists 'replace tmp-path))
    (define in (open-input-file (card-path c)))
    (define header (if (string=? mode "embed") embed-header index-header))
    (displayln header f)
    (copy-port in f)
    (final-card (card-addr c) tmp-path (build-path "_build" (card-addr c) (string-append mode ".html")))))

(define (build c)
  (define addr (final-card-addr c))
  (define src (final-card-path c))
  (define target (final-card-target-path c))


  (define f (open-output-file #:exists 'replace target))
  (process*/ports f (current-input-port) (current-output-port) (find-executable-path "racket") src)

  (printf "build ~s\n" addr)
  )

(define (search-and-build dir)
  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define card-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (card addr path)))

  (copy-file "tr.rkt" "_tmp/tr.rkt")

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)
  (define embed-cards (produce-scrbl card-list "embed"))

  (define output-dir (build-path "_build/"))
  (make-directory* output-dir)

  (for ([c embed-cards])
    (build c))

  (define index-cards (produce-scrbl card-list "index"))

  (for ([c index-cards])
    (build c))
  )

(search-and-build "content")
