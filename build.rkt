#lang racket
(provide search-and-build)
(require dirname
         json
         data/queue
         gregor
         mischief/dict
         mischief/sort)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require "private/common.rkt")

(define (meta-header content)
  (format "#lang scribble/text
@(require tr/metadata)
~a
@generate-metadata[]" content))

(define (embed-header content)
  (format "#lang scribble/text
@(require tr/card)
@article{~a}
" content))
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

(struct final-card (src-path addr path target-path) #:transparent)

(define (compute-addr path)
  (basename (path-replace-extension path "")))

(define (root? addr)
  (string=? addr "index"))

(define (produce-scrbl addr-list addr->path mode)
  (for/list ([addr addr-list])
    (define source-path (hash-ref addr->path addr))
    (define tmp-path
      (if (root? addr)
        (build-path "_tmp" (string-append addr ".scrbl"))
        (build-path "_tmp" (string-append addr "." mode ".scrbl"))))
    (define f (open-output-file #:exists 'replace tmp-path))
    (define in (open-input-file source-path))
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
        [(string=? mode "embed") (build-path "_tmp" (string-append addr "." mode ".html"))]
        [(root? addr) (build-path "_build" (string-append mode ".html"))]
        [else (build-path "_build" addr (string-append mode ".html"))]))
    (final-card source-path addr tmp-path output-path)))

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
  (define addr->path (make-hash))
  (define addr-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (hash-set! addr->path addr path)
      addr))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)

  (define excludes (mutable-set))

  (define meta-cards (produce-scrbl addr-list addr->path "meta"))
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

  (produce-embeds addr-list addr->path excludes addr-maps-to-metajson)

  (define index-cards (produce-scrbl addr-list addr->path "index"))
  (for/async ([c index-cards]
              #:unless (set-member? excludes (final-card-addr c)))
    (printf "generate ~a.index.html ~n" (final-card-addr c))
    (produce-html c))

  (define tex-list (find-files (lambda (x) (path-has-extension? x #".tex")) "_tmp"))
  (for/async ([tex-path tex-list])
    (printf "compile ~a ~n" (path->string tex-path))
    (parameterize ([current-directory (dirname tex-path)]
                   [current-output-port (open-output-string "")])
      (system* (find-executable-path "latex") "-halt-on-error" "-interaction=nonstopmode" "job.tex"))
    (system* (find-executable-path "dvisvgm")
      "-o" (format "_build/~a.svg" (basename (dirname tex-path)))
      (path->string (path-replace-extension tex-path ".dvi"))))

  (produce-search)
  (produce-rss)
  )

(define (produce-embeds addr-list addr->path excludes addr-maps-to-metajson)
  (define neighbors
    (dict->procedure (hash-map/copy addr-maps-to-metajson (λ (addr json) (values addr (hash-ref json 'transclude '()))))))

  (define addr-list* (topological-sort addr-list neighbors))
  (define embed-cards (produce-scrbl addr-list* addr->path "embed"))

  (for/async ([c embed-cards]
              #:unless (set-member? excludes (final-card-addr c)))
    (printf "generate ~a.embed.html ~n" (final-card-addr c))
    (produce-html c))
  )

(define (produce-search)
  (define (itemize items)
    (string-join (for/list ([p items]) (file->string p)) ","))
  (define json-list (find-files (lambda (x) (path-has-extension? x #".metadata.json")) "_tmp"))
  (define out (open-output-file #:exists 'replace "_build/search.json"))
  (displayln "[" out)
  (displayln (itemize json-list) out)
  (displayln "]" out)
  (close-output-port out))

(define/provide-elements/not-empty item pubDate)
(define (produce-rss)
  (define (get-metadata addr)
    (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (define site-obj (file->json "site.json"))
  (define site-url (hash-ref site-obj 'domain))
  (define site-title (hash-ref site-obj 'title))
  (define site-description (hash-ref site-obj 'description))

  (define (itemize items)
    (add-between (for/list ([meta-object items])
                    (define pub-date (iso8601->datetime (hash-ref meta-object 'date)))
                    (item
                      (title (hash-ref meta-object 'title))
                      (link (string-append "https://" (path->string (build-path site-url (hash-ref meta-object 'id)))))
                      (pubDate (~t pub-date "EEE, dd MMM yyyy HH:mm:ss +0800"))))
                 "\n"))
  (define addrs
    (sort
      (for/list ([path (find-files
                         (lambda (x) (path-has-extension? x #".scrbl"))
                         "content/post")])
        (get-metadata (basename (path-replace-extension path ""))))
      (λ (a b)
        (datetime>=? (iso8601->datetime (hash-ref a 'date)) (iso8601->datetime (hash-ref b 'date))))))

  (define out (open-output-file #:exists 'replace "_build/rss.xml"))
  (fprintf out "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<rss version=\"2.0\">
<channel>
  <title>~a</title>
  <link>~a</link>
  <description>~a</description>
  ~a
</channel>
</rss>
" site-title 
  site-url
  site-description
  (xml->string (itemize addrs)))
  (close-output-port out))
