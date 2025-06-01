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
(define root-header "#lang scribble/text
@(require \"tr.rkt\")
@(generate-index #t)
@(generate-root #t)
@(doctype 'html)
")

(struct card (addr path) #:transparent)
(struct final-card (addr path target-path) #:transparent)

(define (compute-addr path)
  (basename (path-replace-extension path "")))

(define (root? addr)
  (string=? addr "index"))

(define (produce-scrbl card-list mode)
  (for/list ([c card-list])
    (define addr (card-addr c))
    (define tmp-path
      (if (root? addr)
        (build-path "_tmp" (string-append addr ".scrbl"))
        (build-path "_tmp" (string-append addr "." mode ".scrbl"))))
    (define f (open-output-file #:exists 'replace tmp-path))
    (define in (open-input-file (card-path c)))
    (define header (cond
      [(root? addr) root-header]
      [(string=? mode "embed") embed-header]
      [else index-header]))
    (displayln header f)
    (copy-port in f)

    (define output-path
      (if (root? addr)
        (build-path "_build" (string-append mode ".html"))
        (build-path "_build" addr (string-append mode ".html"))))
    (final-card addr tmp-path output-path)))

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

  (define out (open-output-file #:exists 'replace "_tmp.sh"))
  (for ([c embed-cards])
    (displayln (build-shell c) out))
  (for ([c index-cards])
    (displayln (build-shell c) out))

  (define tex-list (find-files (lambda (x) (path-has-extension? x #".tex")) "_tmp"))
  (for ([tex-path tex-list])
    (displayln (format "(cd ~a && latex job.tex)" (dirname tex-path)) out)
    (displayln (format "dvisvgm -o _build/~a.svg ~a" (basename (dirname tex-path)) (path->string (path-replace-extension tex-path ".dvi"))) out))

  (close-output-port out)
  
  )

(search-and-build "content")
