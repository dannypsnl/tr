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
         "private/store.rkt"
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

  (define cache-root (build-path tmp "cache"))
  (init-store! cache-root)

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

  #|
  content-addressed build: a card's build signature captures everything its
  rendered output depends on. The canonical store is keyed by that signature,
  so a card is rendered at most once per distinct output: a store hit copies
  the cached artifacts into place, a miss renders and snapshots them.

  This makes a second output target a copy of the first, and reverting a card
  a copy of its still-cached entry.
  |#
  (define sorted-addr-list (topo-order addr-list addr-maps-to-metajson))
  (define signatures
    (compute-signatures sorted-addr-list addr->path addr-maps-to-metajson tmp
                        (render-config-tag)))
  (define embed-cards (produce-scrbl sorted-addr-list addr->path "embed"))
  (define card-of (for/hash ([c embed-cards]) (values (final-card-addr c) c)))

  (for ([addr sorted-addr-list])
    (define sig (hash-ref signatures addr))
    (cond
      [(root? addr)
       (printf "generate ~a.embed.html ~n" addr)
       (produce-html (hash-ref card-of addr))
       (produce-index! addr addr-maps-to-metajson)]
      [(store-hit? cache-root sig)
       ; always refresh the embed into _tmp (a transcluding parent may read it);
       ; the per-target output is rebuilt only when this target's stamp is stale.
       (restore-embed! cache-root sig addr)
       (unless (output-fresh? cache-root sig addr)
         (restore-output! cache-root sig addr)
         (produce-index! addr addr-maps-to-metajson)
         (write-output-stamp! addr sig))]
      [else
       (printf "generate ~a.embed.html ~n" addr)
       (produce-html (hash-ref card-of addr))
       (compile-graphics addr)
       (save-to-store! cache-root sig addr)
       (produce-index! addr addr-maps-to-metajson)
       (write-output-stamp! addr sig)]))

  (produce-search)
  (produce-rss))

; Compile a card's @m/tikz/typst graphics: the embed render emits tex/typ
; sources under _tmp/<addr>/; each becomes an svg under <output>/<addr>/.
(define (compile-graphics addr)
  (define base (build-path "_tmp" addr))
  (when (directory-exists? base)
    (define (svg-target src)
      (string-replace (path->string (path-replace-extension src #".svg"))
                      "_tmp" (get-output-path)))
    (for ([tex-path (find-files (lambda (x) (path-has-extension? x #".tex")) base)])
      (printf "compile ~a ~n" (path->string tex-path))
      (parameterize ([current-directory (dirname tex-path)]
                     [current-output-port (open-output-string "")])
        (system* (find-executable-path "latex")
                 "-halt-on-error"
                 "-interaction=nonstopmode"
                 (basename tex-path)))
      (define svg-path (svg-target tex-path))
      (make-directory* (dirname svg-path))
      (system* (find-executable-path "dvisvgm")
               "--exact"
               "--clipjoin"
               "--font-format=woff"
               "--bbox=papersize"
               "--zoom=1.5"
               "-o" svg-path
               (path->string (path-replace-extension tex-path #".dvi"))))
    (for ([typ-path (find-files (lambda (x) (path-has-extension? x #".typ")) base)])
      (printf "compile ~a ~n" (path->string typ-path))
      (define svg-path (svg-target typ-path))
      (make-directory* (dirname svg-path))
      (system* (find-executable-path "typst")
               "compile"
               "--format" "svg"
               (path->string typ-path)
               svg-path))))

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

(define (produce-search)
  (define (itemize items)
    (string-join (for/list ([p items]) (file->string p)) ","))
  (define json-list (find-files (lambda (x) (path-has-extension? x #".metadata.json")) "_tmp"))
  (define out (open-output-file #:exists 'truncate/replace (build-path (get-output-path) "search.json")))
  (displayln "[" out)
  (displayln (itemize json-list) out)
  (displayln "]" out)
  (close-output-port out))
