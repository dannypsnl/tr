#lang racket
(provide search-and-build)
(require dirname
         mischief/dict
         mischief/sort
         argo/equal)
(require "metadata.rkt"
         "private/common.rkt"
         "private/config.rkt"
         "private/rss.rkt"
         "generate-index.rkt")

(define (embed-header addr content)
  (define rkt-path (build-path "_tmp" (string-append addr ".rkt")))
  (format "#lang scribble/text
@(require tr/card)
~a
@self-addr{~a}
@article{~a}
"
  (if (file-exists? rkt-path)
    (string-append "@(require \"" addr ".rkt\")")
    "")
  addr
  content))

(struct final-card (src-path addr path target-path) #:transparent)

(define (produce-scrbl addr-list addr->path mode)
  (for/list ([addr addr-list])
    (define source-path (hash-ref addr->path addr))
    (define tmp-path
      (cond
        [(root? addr) (build-path "_tmp" (string-append addr ".scrbl"))]
        [else (build-path "_tmp" (string-append addr "." mode ".scrbl"))]))
    (define f (open-output-file #:exists 'truncate/replace tmp-path))
    (define in (open-input-file source-path))
    (displayln
      (embed-header
        addr
        (port->string in))
      f)
    (close-input-port in)
    (close-output-port f)

    (define output-path
      (cond
        [(string=? mode "embed") (build-path "_tmp" (string-append addr "." mode ".html"))]
        [(root? addr) (build-path (get-output-path) (string-append mode ".html"))]
        [else (build-path (get-output-path) addr (string-append mode ".html"))]))
    (final-card source-path addr tmp-path output-path)))

(define (produce-html c)
  (define src (final-card-path c))
  (define target (final-card-target-path c))

  (define out (open-output-file #:exists 'truncate/replace target))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "racket") src))
  (close-output-port out))

(define (search-and-build dir)
  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define addr->path (make-hash))
  (define addr-list
    (for/list ([path scrbl-list])
      (define addr (compute-addr path))
      (hash-set! addr->path addr path)
      addr))

  (when (dev-mode?)
    (write-to-file addr->path (build-path (get-output-path) "sourcemap.rktd")))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)

  (define excludes (mutable-set))

  ; exclude files those no change
  (for/async ([addr addr-list])
    (define meta-path (build-path "_tmp" (string-append addr ".metadata.json")))
    (when (file-exists? meta-path)
      (when (< (file-or-directory-modify-seconds (hash-ref addr->path addr))
               (file-or-directory-modify-seconds meta-path))
        (set-add! excludes addr))))
  ; record all metadata
  (define addr-maps-to-metajson (make-hash))
  ; record their differential updates
  (define metadata-changed (mutable-set)) ; track what's changed
  (define metadata-changes (make-hash)) ; track what's changed

  (for/async ([addr addr-list])
    (define rkt-path (build-path "_tmp" (string-append addr ".rkt")))
    (define lst (compute-racket (hash-ref addr->path addr)))
    (unless (empty? lst)
      (define out (open-output-file #:exists 'truncate/replace rkt-path))
      (for ([text lst])
        (displayln text out))
      (close-output-port out)))
  (for/async ([addr addr-list])
    (define meta-path (build-path "_tmp" (string-append addr ".metadata.json")))
    (cond
      [(not (file-exists? meta-path))
        (define obj (compute-metadata addr (hash-ref addr->path addr)))
        (json->file obj meta-path)
        (hash-set! addr-maps-to-metajson addr obj)]
      [(set-member? excludes addr)
       (hash-set! addr-maps-to-metajson addr (file->json meta-path))]
      [else
        (hash-set! addr-maps-to-metajson addr (compute-metadata addr (hash-ref addr->path addr)))]))
  ; compute relations
  (for/async ([top-addr addr-list])
    (define meta-obj (hash-ref addr-maps-to-metajson top-addr))
    (define related-set (mutable-set))
    (define references-set (mutable-set))

    (for/async ([addr (hash-ref meta-obj 'transclude)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define ctx-set (list->set (hash-ref obj 'context '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'context (set->list (set-add ctx-set top-addr)))))
    (for/async ([addr (hash-ref meta-obj 'related)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->list (set-add links-set top-addr))))
      (match (hash-ref obj 'taxon)
        ["Reference" (set-add! references-set addr)]
        [_ (set-add! related-set addr)]))
    (for/async ([addr (hash-ref meta-obj 'authors)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->list (set-add links-set top-addr)))))

    (hash-set! addr-maps-to-metajson top-addr
      (hash-set* meta-obj
        'related (set->list related-set)
        'references (set->list references-set))))
  (for/async ([addr addr-list])
    (define meta-obj (hash-ref addr-maps-to-metajson addr))
    (define refs (list->mutable-set (hash-ref meta-obj 'references)))

    (for/async ([addr (hash-ref meta-obj 'transclude)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define references (hash-ref obj 'references))
      (for ([ref references])
        (set-add! refs ref)))

    (hash-set! addr-maps-to-metajson addr
      (hash-set* meta-obj 'references (set->list refs))))

  (for/async ([addr addr-list])
    (define new-meta (hash-ref addr-maps-to-metajson addr))
    (define meta-path (build-path "_tmp" (string-append addr ".metadata.json")))
    (when (file-exists? meta-path)
      (define old-meta (file->json meta-path))
      (unless (equal-jsexprs? old-meta new-meta)
        (set-add! metadata-changed addr)

        (define changes (make-hash))
        (for ([key '(transclude related authors context references backlinks)])
          (define old-set (list->set (hash-ref old-meta key '())))
          (define new-set (list->set (hash-ref new-meta key '())))
          (define added (set-subtract new-set old-set))
          (define removed (set-subtract old-set new-set))
          (unless (and (set-empty? added) (set-empty? removed))
            (hash-set! changes key (cons added removed))))
        (unless (hash-empty? changes)
          (hash-set! metadata-changes addr changes)))))

  ; produce/update <addr>.metadata.json
  (set-for-each metadata-changed
    (λ (addr)
      (define json (hash-ref addr-maps-to-metajson addr))
      (printf "update ~a.metadata.json ~n" addr)
      (json->file json (build-path "_tmp" (string-append addr ".metadata.json")))))

  ; Use differential changes to mark precise neighbors for update
  (hash-for-each metadata-changes
    (λ (_ changes)
      (hash-for-each changes
        (λ (_ added-removed-pair)
          (define added (car added-removed-pair))
          (define removed (cdr added-removed-pair))
          ; Mark newly added neighbors for update
          (set-subtract! excludes added)
          ; Mark removed neighbors for update (they need to clean up backlink style links)
          (set-subtract! excludes removed)))))
  (for/async ([addr addr-list]
              #:unless (set-member? excludes addr))
    (define obj (hash-ref addr-maps-to-metajson addr))
    (define ctx (hash-ref obj 'context '()))
    (for ([addr ctx])
      (set-remove! excludes addr)))

  (produce-embeds addr-list addr->path excludes addr-maps-to-metajson)
  (produce-indexes addr-list excludes addr-maps-to-metajson)

  (define tex-list (find-files (lambda (x) (path-has-extension? x #".tex")) "_tmp"))
  (for/async ([tex-path tex-list]
              #:unless (set-member? excludes (basename (dirname tex-path))))
    (printf "compile ~a ~n" (path->string tex-path))
    (parameterize ([current-directory (dirname tex-path)]
                   [current-output-port (open-output-string "")])
      (system* (find-executable-path "latex")
        "-halt-on-error"
        "-interaction=nonstopmode"
        (basename tex-path)))

    (define svg-path
      (string-replace (path->string (path-replace-extension tex-path #".svg"))
        "_tmp"
        (get-output-path)))
    (system* (find-executable-path "dvisvgm")
      "--exact"
      "--clipjoin"
      "--font-format=woff"
      "--bbox=papersize"
      "--zoom=1.5"
      "-o" svg-path
      (path->string (path-replace-extension tex-path #".dvi"))))

  (define typ-list (find-files (lambda (x) (path-has-extension? x #".typ")) "_tmp"))
  (for/async ([typ-path typ-list]
              #:unless (set-member? excludes (basename (dirname typ-path))))
    (printf "compile ~a ~n" (path->string typ-path))
    (define svg-path
      (string-replace (path->string (path-replace-extension typ-path #".svg"))
        "_tmp"
        (get-output-path)))
    (make-directory* (dirname svg-path))
    (system* (find-executable-path "typst")
      "compile"
      "--format" "svg"
      (path->string typ-path)
      svg-path))

  (produce-search)
  (produce-rss))

(define (produce-embeds addr-list addr->path excludes addr-maps-to-metajson)
  (define neighbors
    (dict->procedure (hash-map/copy addr-maps-to-metajson (λ (addr json) (values addr (hash-ref json 'transclude '()))))))

  (define addr-list* (topological-sort addr-list neighbors))
  (define embed-cards (produce-scrbl addr-list* addr->path "embed"))

  (for/async ([c embed-cards]
              #:unless (set-member? excludes (final-card-addr c)))
    (printf "generate ~a.embed.html ~n" (final-card-addr c))
    (produce-html c)))

(define (produce-search)
  (define (itemize items)
    (string-join (for/list ([p items]) (file->string p)) ","))
  (define json-list (find-files (lambda (x) (path-has-extension? x #".metadata.json")) "_tmp"))
  (define out (open-output-file #:exists 'truncate/replace (build-path (get-output-path) "search.json")))
  (displayln "[" out)
  (displayln (itemize json-list) out)
  (displayln "]" out)
  (close-output-port out))
