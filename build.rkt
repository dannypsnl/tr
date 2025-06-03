#lang racket
(require dirname)

(define (embed-header _ content)
  (format "#lang scribble/text
@(require \"tr.rkt\")
@(doctype 'html)
@common-share{
  @div['class: \"top-wrapper\"]{
    @tree{~a}
  }
}" content))
(define (index-header addr content)
  (format "#lang scribble/text
@(require \"tr.rkt\")
@(generate-index? #t)
@(doctype 'html)
@common-share{
  @div['class: \"top-wrapper\"]{
    @tree{~a}
    @generate-toc[]
  }
  @footer{
    @details['open: #t 'id: \"context\"]{
      @summary{@h2{Context}}
      @include{~a.context.scrbl}
    }
    @details['open: #t 'id: \"backlinks\"]{
      @summary{@h2{Backlinks}}
      @include{~a.backlinks.scrbl}
    }
    @generate-related[]
  }
  @generate-metadata[]
}" content addr addr))
(define (root-header _ content)
  (format "#lang scribble/text
@(require \"tr.rkt\")
@(generate-index? #t)
@(generate-root? #t)
@(doctype 'html)
@common-share{
  @div['class: \"top-wrapper\"]{
    @tree{~a}
  }
}" content))

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
    (displayln (header addr (port->string in)) f)
    (close-input-port in)
    (close-output-port f)

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
    (define ctx-path (build-path "_tmp" (string-append (final-card-addr c) "." "context" ".scrbl")))
    (define backlink-path (build-path "_tmp" (string-append (final-card-addr c) "." "backlinks" ".scrbl")))
    ; before run xxx.embed.scrbl, we create xxx.context.scrbl (use `touch`) for each address xxx
    (displayln (string-append "touch " (path->string ctx-path)) out)
    ; before run xxx.embed.scrbl, we create xxx.backlinks.scrbl (use `touch`) for each address xxx
    (displayln (string-append "touch " (path->string backlink-path)) out))
  (for ([c embed-cards])
    #|
      run `xxx.embed.scrbl` will create a series of side effects
      1. update yyy.context.scrbl if it transclude{yyy}
      2. update yyy.backlinks.scrbl if it link{yyy}
      to help us create final output (index.html)

      It also create embed.html that others card can use iframe to refer them.
    |#
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
