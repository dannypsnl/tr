#lang racket/base
(provide setup-config!
         get-config
         get-output-path
         get-assets-path
         get-extension-module
         remove-scrbl?
         run-after-build!
         dev-mode?
         render-content-config-tag
         render-shell-config-tag)
(require racket/file
         racket/path
         scribble/html/xml
         "common.rkt")

(define configuration #f)

#|
Config is a Racket module (site.rkt) that `(provide site)` a hash.

Old `.json` config is no longer a runtime format: seeing one trigger a one-time
migration: we generate the equivalent site.rkt, then suggest deleting the .json,
and then load the freshly written .rkt so this very build keeps working.
|#
(define (setup-config! filepath)
  (define path-str (if (path? filepath) (path->string filepath) filepath))
  (define rkt-cfg-path (path-replace-extension filepath #".rkt"))
  (unless (or (file-exists? filepath) (file-exists? rkt-cfg-path))
    (raise-user-error 'tr
                      (string-append
                        "no site configuration found\n"
                        "  looked for: ~a\n"
                        "  hint: `raco tr init` writes one, or pass -c <config>")
                      rkt-cfg-path))
  (cond
    [(path-has-extension? filepath #".rkt") (void)]
    [(and (path-has-extension? filepath #".json")
          (file-exists? rkt-cfg-path))
     (eprintf "tr: ~a is already existed, please use it and delete ~a (JSON config is ignored by tr)\n"
              rkt-cfg-path path-str)]
    [else
     ; migrate old JSON configuration to new racket configuration
     (call-with-output-file rkt-cfg-path
       (lambda (out) (upgrade-json-config! (file->json filepath) out)))
     (eprintf "tr: already produce ~a from ~a - please use ~a and delete ~a\n"
              rkt-cfg-path path-str rkt-cfg-path path-str)])

  (set! configuration (dynamic-require (path->complete-path rkt-cfg-path) 'site)))

; Upgrade old JSON config as a site.rkt source module.
(define (upgrade-json-config! cfg out)
  (define (emit-value v)
    (if (or (string? v) (number? v) (boolean? v))
        (format "~s" v)
        (format "'~s" v))) ; arrays, null symbol, ...
  (fprintf out "#lang racket/base\n(require scribble/html)\n(provide site)\n\n")
  (fprintf out "(define site\n  (hash")
  (define first? #t)
  (for ([k (sort (hash-keys cfg) symbol<?)]
        #:unless (eq? k 'fedi))
    (fprintf out "~a'~a ~a" (if first? " " "\n        ") k (emit-value (hash-ref cfg k)))
    (set! first? #f))
  ; the legacy `fedi` key is expanded into the two head) elements it used to inject (rel=me link + fediverse:creator meta)
  (define fedi (hash-ref cfg 'fedi #f))
  (when fedi
    (define site* (hash-ref fedi 'site))
    (define handle (hash-ref fedi 'handle))
    (fprintf out
             (string-append "\n        'head (list (link 'rel: \"me\" 'href: ~s)"
                            "\n                    (meta 'name: \"fediverse:creator\" 'content: ~s))")
             (format "https://~a/@~a" site* handle)
             (format "@~a@~a" handle site*)))
  (fprintf out "))\n"))

(define (get-config key default)
  (hash-ref configuration key default))

(define (get-output-path)
  (get-config 'output-path "_build"))

(define (get-assets-path)
  (get-config 'assets '("assets")))

(define (get-extension-module)
  (define rel (get-config 'extension-module #f))
  (and rel (path->complete-path rel)))

#|
remove-scrbl? is a predicate on source path of a card.

When it returns #t, it eliminate the card from the further pipeline.
|#
(define (remove-scrbl? source-path)
  (define p? (get-config 'remove-content-if #f))
  (if p?
      (p? source-path)
      #f))

(define (run-after-build!)
  (define hook (get-config 'after-build #f))
  (when hook
    (hook (get-output-path))))

(define (dev-mode?)
  (equal? "dev" (get-config 'mode "release")))

; Two stable strings of the config that the renderer bakes into output, split
; by *which* output each key reaches. Both feed build signatures, so a config
; change that changes rendered output forces the matching rebuild instead of
; leaving stale files in place.
;
; content: config baked into a card's embed.html (and hence its compiled
; graphics), which is what the cross-target content store is keyed by. Only
; `extension-module` today (see build.rkt's embed-header); it is keyed on its
; file content, not its path, so editing a card's macros invalidates the store
; even though no .scrbl content hash changed.
(define (render-content-config-tag)
  (define ext-mod (get-extension-module))
  (format "~s" (list (and ext-mod (file-exists? ext-mod) (file->bytes ext-mod)))))

; shell: config that only reaches the page shell produce-index! wraps around
; an already-rendered embed -- `head`, `header`, `html-lang` (see
; generate-index.rkt). Kept out of the store key on purpose: editing a site's
; header must re-emit every index.html, but it must not invalidate a single
; embed and re-run its LaTeX/Typst. Hence it keys only the per-target `.sig`
; stamp (see store.rkt), which is what decides whether produce-index! reruns.
; `head`/`header` are keyed on their rendered bytes (xml->string) rather than
; the element values, which print opaquely and unstably.
(define (render-shell-config-tag)
  (format "~s" (list (map xml->string (get-config 'head '()))
                     (xml->string (get-config 'header '()))
                     (get-config 'html-lang ""))))
