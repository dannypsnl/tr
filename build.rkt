#lang racket
(require racket/system
        dirname)

(define embed-header "#lang scribble/text
@(require \"tr.rkt\")
@(doctype 'html)
")
(define index-header "#lang scribble/text
@(require \"tr.rkt\")
@(generate-index #t)
@(doctype 'html)
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

(define (build-shell c)
  (define addr (final-card-addr c))
  (define src (final-card-path c))
  (define target (final-card-target-path c))

  (define output-dir (build-path "_build/" addr))
  (make-directory* output-dir)

  (format "racket ~a > ~a" (path->string src) (path->string target)))

(define (search-and-build dir)
  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define card-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (card addr path)))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)
  (copy-file "tr.rkt" "_tmp/tr.rkt" #t)

  (define embed-cards (produce-scrbl card-list "embed"))
  (define index-cards (produce-scrbl card-list "index"))

  (define out (open-output-file #:exists 'replace ".tmp.sh"))
  (for ([c embed-cards])
    (displayln (build-shell c) out))
  (for ([c index-cards])
    (displayln (build-shell c) out))
  )

(search-and-build "content")
