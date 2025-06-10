#lang racket
(provide search-and-build)
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
    @main{@tree{@article{~a}}}
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
(struct final-card (src-path addr path target-path) #:transparent)

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
    (final-card (card-path c) addr tmp-path output-path)))

(define (copy-directory-recursively source-dir target-dir)
  (make-directory* target-dir)
  (for ([item (directory-list source-dir)])
    (if (string=? ".git" (path->string item))
      (void)
      (let ([source-path (build-path source-dir item)]
            [target-path (build-path target-dir item)])
        (if (directory-exists? source-path)
            (copy-directory-recursively source-path target-path)
            (copy-file source-path target-path #t))))))

(define (produce-html c)
  (define addr (final-card-addr c))
  (define src (final-card-path c))
  (define target (final-card-target-path c))

  (define output-dir (build-path "_build/" addr))
  (make-directory* output-dir)

  (define out (open-output-file #:exists 'replace target))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "racket") src))
  (close-output-port out))

(define (produce file from-file)
  (printf "racket ~a > ~a~n" from-file file)
  (define out (open-output-file #:exists 'replace file))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "racket") from-file))
  (close-output-port out))

(define (search-and-build dir)
  (copy-directory-recursively "assets" "_build")

  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define card-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (card addr path)))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)

  (define excludes (mutable-set))

  (define meta-cards (produce-scrbl card-list "meta"))
  ; exclude files those no change
  (for/async ([c meta-cards])
    (define meta-path (build-path "_tmp" (format "~a.metadata.json" (final-card-addr c))))
    (when (file-exists? meta-path)
      (when (< (file-or-directory-modify-seconds (final-card-src-path c))
               (file-or-directory-modify-seconds meta-path))
        (set-add! excludes (final-card-addr c)))))
  ; record all metadata
  (define addr-maps-to-metajson (make-hash))
  (for/async ([c meta-cards])
    (if (set-member? excludes (final-card-addr c))
      (hash-set! addr-maps-to-metajson (final-card-addr c) (file->json (final-card-target-path c)))
      (begin
        (printf "generate ~a.metadata.json ~n" (final-card-addr c))
        (parameterize ([current-output-port (open-output-string)])
          (system* (find-executable-path "racket") (final-card-path c)))
        (hash-set! addr-maps-to-metajson (final-card-addr c) (file->json (final-card-target-path c))))))
  ; compute relations
  (for/async ([c meta-cards])
    (define meta-obj (hash-ref addr-maps-to-metajson (final-card-addr c)))
    (define related-queue (make-queue))
    (define references-queue (make-queue))

    (for/async ([addr (hash-ref meta-obj 'transclude)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define ctx-set (list->set (hash-ref obj 'context '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'context (set->list (set-add ctx-set (final-card-addr c))))))
    (for/async ([addr (hash-ref meta-obj 'related)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->list (set-add links-set (final-card-addr c)))))
      (match (hash-ref obj 'taxon)
        ["Reference" (enqueue! references-queue addr)]
        [_ (enqueue! related-queue addr)]))
    (for/async ([addr (hash-ref meta-obj 'authors)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->list (set-add links-set (final-card-addr c))))))

    (hash-set! addr-maps-to-metajson (final-card-addr c) (hash-set* meta-obj 'related (queue->list related-queue) 'references (queue->list references-queue))))
  (for/async ([c meta-cards])
    (define meta-obj (hash-ref addr-maps-to-metajson (final-card-addr c)))
    (define references-queue (make-queue))

    (for/async ([addr (hash-ref meta-obj 'transclude)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define references (hash-ref obj 'references))
      (for ([ref references])
        (enqueue! references-queue ref)))

    (hash-set! addr-maps-to-metajson (final-card-addr c)
      (hash-set* meta-obj
        'references
        (append (hash-ref meta-obj 'references) (queue->list references-queue)))))
  ; produces <addr>.metadata.json
  (hash-for-each addr-maps-to-metajson
    (λ (addr json)
      (json->file json (build-path "_tmp" (string-append addr "." "metadata" ".json")))))

  (define embed-cards (produce-scrbl card-list "embed"))
  (define index-cards (produce-scrbl card-list "index"))

  (for/async ([c embed-cards]
              #:unless (set-member? excludes (final-card-addr c)))
    (printf "generate ~a.embed.html ~n" (final-card-addr c))
    (produce-html c))
  (for/async ([c index-cards]
              #:unless (set-member? excludes (final-card-addr c)))
    (printf "generate ~a.index.html ~n" (final-card-addr c))
    (produce-html c))

  (define tex-list (find-files (lambda (x) (path-has-extension? x #".tex")) "_tmp"))
  (for/async ([tex-path tex-list])
    (printf "compile ~a ~n" (path->string tex-path))
    (parameterize ([current-directory (dirname tex-path)]
                   [current-output-port (open-output-string "")])
      (system* (find-executable-path "latex") "job.tex"))
    (system* (find-executable-path "dvisvgm")
      "-o" (format "_build/~a.svg" (basename (dirname tex-path)))
      (path->string (path-replace-extension tex-path ".dvi"))))
  
  (produce-search)
  (produce "_build/rss.xml" "rss.scrbl")
  )

(define (produce-search)
  (define (itemize items)
    (string-join (map file->string items) ","))
  (define json-list (find-files (lambda (x) (path-has-extension? x #".metadata.json")) "_tmp"))
  (define out (open-output-file #:exists 'replace "_build/search.json"))
  (displayln "[" out)
  (displayln (itemize json-list) out)
  (displayln "]" out)
  (close-output-port out))
