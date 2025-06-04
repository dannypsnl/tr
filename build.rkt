#lang racket
(require dirname json data/queue)
(require "private/common.rkt")

(define (meta-header content)
  (format "#lang scribble/text
@(require tr/metadata)
~a
@generate-metadata[]" content))

(define (embed-header content)
  (format "#lang scribble/text
@(require tr/card)
@(doctype 'html)
@common-share{
  @article{~a}
}" content))
(define (index-header content)
  (format "#lang scribble/text
@(require tr/card)
@(generate-index? #t)
@(doctype 'html)
@common-share{
  @div['class: \"top-wrapper\"]{
    @tree{@article{~a}}
    @generate-toc[]
  }
  @footer{
    @generate-context[]
    @generate-references[]
    @generate-backlinks[]
    @generate-related[]
  }
}" content))
(define (root-header content)
  (format "#lang scribble/text
@(require tr/card)
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
      [(string=? mode "meta") meta-header]
      [(root? addr) root-header]
      [(string=? mode "embed") embed-header]
      [else index-header]))
    (displayln (header (port->string in)) f)
    (close-input-port in)
    (close-output-port f)

    (define output-path
      (cond
        [(string=? mode "meta") (build-path "_tmp" (string-append addr "." "metadata" ".json"))]
        [(root? addr) (build-path "_build" (string-append mode ".html"))]
        [else (build-path "_build" addr (string-append mode ".html"))]))
    (final-card addr tmp-path output-path)))

(define (build-shell c)
  (define addr (final-card-addr c))
  (define src (final-card-path c))
  (define target (final-card-target-path c))

  (define output-dir (build-path "_build/" addr))
  (make-directory* output-dir)

  (define out (open-output-file #:exists 'replace target))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "racket") src))
  (close-output-port out))

(define (search-and-build dir)
  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define card-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (card addr path)))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)

  (define meta-cards (produce-scrbl card-list "meta"))
  ; produces basic <addr>.metadata.json
  (for/async ([c meta-cards])
    (printf "generate ~a.metadata.json ~n" (final-card-addr c))
    (parameterize ([current-output-port (open-output-string "")])
      (system* (find-executable-path "racket") (final-card-path c))))
  ; compute relations
  (for/async ([c meta-cards])
    (define meta-obj (file->json (final-card-target-path c)))
    (define related-queue (make-queue))
    (define references-queue (make-queue))
    
    (for ([addr (hash-ref meta-obj 'transclude)])
      (define obj (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
      (define out (open-output-file #:exists 'replace (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
      (define new-ctx (cons (final-card-addr c) (hash-ref obj 'context '())))
      (write-json (hash-set obj 'context new-ctx) out)
      (close-output-port out))
    (for ([addr (hash-ref meta-obj 'related)])
      (define obj (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
      (define out (open-output-file #:exists 'replace (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
      (define new-backlinks (cons (final-card-addr c) (hash-ref obj 'backlinks '())))
      (write-json (hash-set obj 'backlinks new-backlinks) out)
      (close-output-port out)

      (match (hash-ref obj 'taxon)
        ["Reference" (enqueue! references-queue addr)]
        [_ (enqueue! related-queue addr)]))

    (define out (open-output-file #:exists 'replace (build-path "_tmp" (string-append (final-card-addr c) "." "metadata" ".json"))))
    (write-json	(hash-set* meta-obj 'related (queue->list related-queue) 'references (queue->list references-queue)) out)
    (close-output-port out))

  (define embed-cards (produce-scrbl card-list "embed"))
  (define index-cards (produce-scrbl card-list "index"))

  (for/async ([c embed-cards])
    (printf "generate ~a.embed.html ~n" (final-card-addr c))
    (build-shell c))
  (for/async ([c index-cards])
    (printf "generate ~a.index.html ~n" (final-card-addr c))
    (build-shell c))

  (define tex-list (find-files (lambda (x) (path-has-extension? x #".tex")) "_tmp"))
  (for/async ([tex-path tex-list])
    (printf "compile ~a ~n" (path->string tex-path))
    (parameterize ([current-directory (dirname tex-path)]
                   [current-output-port (open-output-string "")])
      (system* (find-executable-path "latex") "job.tex"))
    (system* (find-executable-path "dvisvgm")
      "-o" (format "_build/~a.svg" (basename (dirname tex-path)))
      (path->string (path-replace-extension tex-path ".dvi"))))
  )

(search-and-build "content")
