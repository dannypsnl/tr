#lang racket
(provide search-and-build)
(require racket/rerequire)
(require dirname
         file/sha1
         json
         mischief/dict
         mischief/sort
         argo/equal)
(require "../card.rkt"
         "metadata.rkt"
         "metadata-store.rkt"
         "common.rkt"
         (prefix-in config: "config.rkt")
         "signature.rkt"
         "store.rkt"
         "generate-index.rkt")

; set->list has no stable order, so serializing it directly makes metadata
; changed every build and the detector keeps re-flagging the addr. Use
; sorted list to avoid that.
(define (set->sorted-list s)
  (sort (set->list s) string<?))

(define (embed-header addr content)
  (define rkt-path (build-path "_tmp" (string-append addr ".rkt")))
  (define ext-mod (config:get-extension-module))
  (define extra-requires
    (string-append
      (if ext-mod
          (format "@(require (file ~s))\n" (path->string ext-mod))
          "")
      (if (file-exists? rkt-path)
          (string-append "@(require \"" addr ".rkt\")")
          "")))
  (format "#lang scribble/text
@(require tr/card)
~a
@self-addr{~a}
@article['class: \"tr-body\"]{~a}
"
          extra-requires
          addr
          content))

(struct final-card (src-path addr path target-path) #:transparent)

(define (produce-scrbl addr-list addr->path mode)
  (define (tmp-scrbl-path addr content-hash mode)
    (build-path "_tmp"
                (string-append addr "-" content-hash "." mode ".scrbl")))
  #|
  dynamic-rerequire's own staleness check (racket/rerequire.rkt: check-latest)
  is `(ts . > . mod-timestamp)`, entirely independent of tr's own content-hash
  signature system.

  Two rebuilds of the same addr within one wall-clock period
  (plausible under `tr watch`, e.g. an editor's format-on-save chain) can
  leave its tmp mtime unchanged even though the content genuinely differs,
  so dynamic-rerequire serves a stale render even though tr already
  correctly decided this addr needs rebuilding.

  Naming the tmp file after its own content hash sidesteps mtime comparison
  entirely: distinct content is always making a new path dynamic-rerequire
  has never seen (hence guaranteed a fresh load).
  |#
  (define (content-hash-of content)
    (sha1 (open-input-bytes (string->bytes/utf-8 content))))
  (define (marker-path addr mode)
    (build-path "_tmp" (string-append addr "." mode ".hash")))

  (for/list ([addr addr-list])
    (define source-path (hash-ref addr->path addr))
    (define content
      (embed-header addr (call-with-input-file source-path port->string)))
    (define content-hash (content-hash-of content))
    (define tmp-path (tmp-scrbl-path addr content-hash mode))
    (define marker (marker-path addr mode))
    (unless (file-exists? tmp-path)
      (when (file-exists? marker)
        (define old-hash (file->string marker))
        (define old-tmp-scrbl-path (tmp-scrbl-path addr old-hash mode))
        (when (file-exists? old-tmp-scrbl-path) (delete-file old-tmp-scrbl-path)))
      (call-with-output-file tmp-path
        (lambda (f) (displayln content f)))
      (call-with-output-file marker #:exists 'replace
        (lambda (f) (display content-hash f))))

    (define output-path
      (cond
        [(string=? mode "embed") (build-path "_tmp" (string-append addr "." mode ".html"))]
        [(root? addr) (build-path (config:get-output-path) (string-append mode ".html"))]
        [else (build-path (config:get-output-path) addr (string-append mode ".html"))]))
    (final-card source-path addr tmp-path output-path)))

(define (produce-html c)
  (define src (final-card-path c))
  (define target (final-card-target-path c))

  (define out (open-output-file #:exists 'truncate/replace target))
  (parameterize ([current-output-port out]
                 [card-counting 0])
    (dynamic-rerequire (path->complete-path src) #:verbosity 'none))
  (close-output-port out))

(define (search-and-build dir)
  (reset-metadata-cache!)

  (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) dir))
  (define addr->path (make-hash))
  (define addr-list
    (for/list ([path scrbl-list]
               #:unless (config:remove-scrbl? path))
      (define addr (compute-addr path))
      (hash-set! addr->path addr path)
      addr))

  (when (config:dev-mode?)
    (with-output-to-file
      #:exists 'truncate/replace
      (build-path (config:get-output-path) "sourcemap.json")
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
  (open-metadata-store! tmp)

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

    (for/async ([addr (transclude-deps meta-obj)])
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

    (for/async ([addr (transclude-deps meta-obj)])
      (define obj (hash-ref addr-maps-to-metajson addr))
      (define references (hash-ref obj 'references))
      (for ([ref references])
        (set-add! refs ref)))

    (hash-set! addr-maps-to-metajson addr
               (hash-set* meta-obj 'references (set->sorted-list refs))))

  ; one transaction for all addrs, not for/async: SQLite serializes writers anyway
  (with-metadata-transaction
    (lambda ()
      (for ([addr addr-list])
        (define new-meta (hash-ref addr-maps-to-metajson addr))
        (define existing (metadata-store-ref addr))
        (unless (and existing
                     (equal-jsexprs? existing new-meta))
          (printf "update ~a.metadata ~n" addr)
          (metadata-store-set! addr new-meta)))))

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
                        (config:render-config-tag)))
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

  (config:run-after-build!)
  (close-metadata-store!))

; Homebrew installs dvisvgm in its own cellar prefix, separate from TeX Live.
; dvisvgm's bundled kpathsea searches for texmf.cnf relative to the dvisvgm
; binary, so it never finds TeX Live's config -> "none of the default map
; files could be found" and font embedding fails. Point kpathsea at TeX Live
; by deriving the locations from TeX Live's own kpsewhich.
(define (setup-dvisvgm-texmf!)
  (define kpsewhich (find-executable-path "kpsewhich"))
  (define (kpse . args)
    (and kpsewhich
         (let ([out (with-output-to-string
                      (lambda () (apply system* kpsewhich args)))])
           (string-trim out))))
  (define cnf (kpse "texmf.cnf"))
  (define root (kpse "--var-value=TEXMFROOT"))
  (when (and cnf (not (string=? cnf "")))
    ; TEXMFCNF wants the directory containing texmf.cnf.
    (define-values (dir _name _dir?) (split-path (string->path cnf)))
    (putenv "TEXMFCNF" (path->string dir)))
  (when (and root (not (string=? root "")))
    (putenv "TEXMFROOT" root)))

; kpsewhich is a subprocess spawn; compile-graphics runs once per built card,
; so calling this unconditionally there was 2 subprocess spawns per card even
; for the vast majority of cards with no tex graphics at all. Env vars only
; need setting once per build, and only when a .tex actually needs compiling.
(define dvisvgm-texmf-ready
  (delay (setup-dvisvgm-texmf!)))

; Compile a card's @m/tikz/typst graphics: the embed render emits tex/typ
; sources under _tmp/<addr>/; each becomes an svg under <output>/<addr>/.
(define (compile-graphics addr)
  (define base (build-path "_tmp" addr))
  (when (directory-exists? base)
    (define (svg-target src)
      (string-replace (path->string (path-replace-extension src #".svg"))
                      "_tmp" (config:get-output-path)))
    (for ([tex-path (find-files (lambda (x) (path-has-extension? x #".tex")) base)])
      (force dvisvgm-texmf-ready)
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

; A card's top-level 'transclude also lists local `parent:N` refs for each
; nested @tr/card block; the real children of those blocks live in
; locals[N].transclude. Expand those local refs into the non-local addrs they
; embed, so a child transcluded only inside an @tr/card still sorts before its
; parent (otherwise the parent's embed reads a not-yet-generated embed.html).
(define (transclude-deps json)
  (define locals (hash-ref json 'locals '()))
  (append*
    (for/list ([addr (hash-ref json 'transclude '())])
      (cond
        [(non-local? addr) (list addr)]
        [else
         (define idx (string->number (second (string-split addr ":"))))
         (if (and idx (< idx (length locals)))
             (filter non-local? (hash-ref (list-ref locals idx) 'transclude '()))
             '())]))))
; topological order (based on transclude): a transcluded child sorts before
; the parent that embeds it. Used both to compute build signatures
; (a child's signature must exist before its parent's) and to generate embeds
; (a parent's embed reads its children's embed.html).
(define (topo-order addr-list addr-maps-to-metajson)
  (define neighbors
    (dict->procedure (hash-map/copy addr-maps-to-metajson
                                    (λ (addr json)
                                      (values addr (transclude-deps json))))))
  (remove-duplicates (topological-sort addr-list neighbors)))

