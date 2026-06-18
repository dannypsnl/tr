#lang racket
(provide search-and-build)
(require racket/runtime-path
         racket/rerequire)
(require dirname
         json
         mischief/dict
         mischief/sort
         argo/equal)
(require "card.rkt"
         "metadata.rkt"
         "private/common.rkt"
         "private/config.rkt"
         "private/rss.rkt"
         "private/signature.rkt"
         "generate-index.rkt")

; set->list has no stable order, so serializing it directly makes metadata
; changed every build and the detector keeps re-flagging the addr. Use
; sorted list to avoid that.
(define (set->sorted-list s)
  (sort (set->list s) string<?))

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
  (parameterize ([current-output-port out]
                 [card-counting 0])
    (dynamic-rerequire (path->complete-path src)))
  (close-output-port out))

(define (search-and-build dir)
  (reset-metadata-cache!)

  (make-directory* (build-path dir "private"))

  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define private-scrbl-list
    (find-files 
      (lambda (x) (path-has-extension? x #".scrbl"))
      (build-path dir "private")))
  (define addr->path (make-hash))
  (define addr-list
    (cond
      [(equal? "release" (get-build-mode))
       (define private-files (list->set private-scrbl-list))
       (for/list ([path scrbl-list]
                  #:when (not (set-member? private-files path)))
          (define addr (compute-addr path))
          (hash-set! addr->path addr path)
          addr)]
      [else
       (for/list ([path scrbl-list])
          (define addr (compute-addr path))
          (hash-set! addr->path addr path)
          addr)]))

  (when (dev-mode?)
    (with-output-to-file
      #:exists 'truncate/replace
      (build-path (get-output-path) "sourcemap.json")
      (lambda ()
        (printf "{")
        (printf
          (string-join
            (hash-map addr->path
                      (lambda (key value)
                        (format "~s: ~s" key (path->string (path->complete-path value)))))
            ","))
        (printf "}"))))

  (define tmp (build-path "_tmp"))
  (make-directory* tmp)

  (define cache-dir (build-path tmp "cache"))
  (make-directory* cache-dir)

  ; emit per-card racket helpers extracted from @tr/code forms
  (for/async ([addr addr-list])
    (define rkt-path (build-path "_tmp" (string-append addr ".rkt")))
    (define lst (compute-racket (hash-ref addr->path addr)))
    (unless (empty? lst)
      (define out (open-output-file #:exists 'truncate/replace rkt-path))
      (for ([text lst])
        (displayln text out))
      (close-output-port out)))
  (define addr-maps-to-metajson (make-hash))
  (for/async ([addr addr-list])
    (hash-set! addr-maps-to-metajson addr
               (compute-metadata addr (hash-ref addr->path addr))))
  ; compute relations
  (for/async ([top-addr addr-list])
    (define meta-obj (hash-ref addr-maps-to-metajson top-addr))
    (define related-set (mutable-set))
    (define references-set (mutable-set))

    (for/async ([addr (hash-ref meta-obj 'transclude)]
                #:when (non-local? addr))
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define ctx-set (list->set (hash-ref obj 'context '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'context (set->sorted-list (set-add ctx-set top-addr)))))
    (for/async ([addr (hash-ref meta-obj 'related)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->sorted-list (set-add links-set top-addr))))
      (match (hash-ref obj 'taxon)
        ["Reference" (set-add! references-set addr)]
        [_ (set-add! related-set addr)]))
    (for/async ([addr (hash-ref meta-obj 'authors)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define links-set (list->set (hash-ref obj 'backlinks '())))
      (hash-set! addr-maps-to-metajson addr (hash-set obj 'backlinks (set->sorted-list (set-add links-set top-addr)))))

    (hash-set! addr-maps-to-metajson top-addr
               (hash-set* meta-obj
                          'related (set->sorted-list related-set)
                          'references (set->sorted-list references-set))))
  (for/async ([addr addr-list])
    (define meta-obj (hash-ref addr-maps-to-metajson addr))
    (define refs (list->mutable-set (hash-ref meta-obj 'references)))

    (for/async ([addr (hash-ref meta-obj 'transclude)]
                #:when (non-local? addr))
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define references (hash-ref obj 'references))
      (for ([ref references])
        (set-add! refs ref)))

    (hash-set! addr-maps-to-metajson addr
               (hash-set* meta-obj 'references (set->sorted-list refs))))

  ; update <addr>.metadata.json if computed metadata differs from disk
  (for/async ([addr addr-list])
    (define new-meta (hash-ref addr-maps-to-metajson addr))
    (define meta-path (build-path "_tmp" (string-append addr ".metadata.json")))
    ; an interrupted build can leave an empty/corrupt metadata file (json->file
    ; truncates before writing); read-json then yields <eof>, which is not a
    ; jsexpr?. Treat any unreadable existing file as a cache miss and rewrite it.
    (define existing
      (and (file-exists? meta-path)
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (define obj (file->json meta-path))
             (and (jsexpr? obj) obj))))
    (unless (and existing
                 (equal-jsexprs? existing new-meta))
      (printf "update ~a.metadata.json ~n" addr)
      (json->file new-meta meta-path)))

  ; content-addressed invalidation: a card is excluded from rebuild iff its
  ; build signature matches the marker its last successful build's cache
  ; AND its outputs are still on disk.
  ;
  ; Any neighbor change that affects a card's output flows into that card's signature.
  (define addr-list* (topo-order addr-list addr-maps-to-metajson))
  (define signatures (compute-signatures addr-list* addr->path addr-maps-to-metajson tmp))
  (define cache-map (read-cache-map cache-dir))
  (define excludes (mutable-set))
  ; the output-existence guard keeps a partial/interrupted build (marker
  ; present, html missing) from excluding the addr forever
  (for ([addr addr-list])
    (when (and (cache-hit? cache-map addr (hash-ref signatures addr))
               (file-exists? (build-path "_tmp" (string-append addr ".embed.html")))
               (file-exists? (index-output-path addr)))
      (set-add! excludes addr)))
  ; tex/typ graphics live under _tmp; if a leftover source's svg is gone from the
  ; output dir, its owning addr must be rebuilt too
  (for ([gfx (append (find-files (λ (p) (path-has-extension? p #".tex")) "_tmp")
                     (find-files (λ (p) (path-has-extension? p #".typ")) "_tmp"))])
    (define svg-path
      (string-replace (path->string (path-replace-extension gfx #".svg"))
                      "_tmp" (get-output-path)))
    (unless (file-exists? svg-path)
      (set-remove! excludes (basename (dirname gfx)))))

  (produce-embeds addr-list* addr->path excludes)
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
  (produce-rss)

  ; record successful builds: write a fresh signature marker for every card we
  ; (re)built whose outputs are now present, replacing any stale marker. Written
  ; last so an interrupted build never leaves a marker without its output.
  (for ([addr addr-list]
        #:unless (set-member? excludes addr))
    (when (and (file-exists? (build-path "_tmp" (string-append addr ".embed.html")))
               (file-exists? (index-output-path addr)))
      (clear-addr-markers! cache-dir addr)
      (write-cache-marker! cache-dir addr (hash-ref signatures addr)))))

; topological order (based on transclude): a transcluded child sorts before
; the parent that embeds it. Used both to compute build signatures
; (a child's signature must exist before its parent's) and to generate embeds
; (a parent's embed reads its children's embed.html).
(define (topo-order addr-list addr-maps-to-metajson)
  (define neighbors
    (dict->procedure (hash-map/copy addr-maps-to-metajson
                                    (λ (addr json)
                                      (values addr
                                              (filter non-local? (hash-ref json 'transclude '())))))))
  (remove-duplicates (topological-sort addr-list neighbors)))

(define (produce-embeds addr-list* addr->path excludes)
  (define embed-cards (produce-scrbl addr-list* addr->path "embed"))

  (for ([c embed-cards]
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
