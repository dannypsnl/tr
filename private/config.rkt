#lang racket/base
(provide setup-config!
         get-config
         get-output-path
         get-assets-path
         get-build-mode
         dev-mode?
         render-config-tag)
(require racket/path
         scribble/html/xml
         "common.rkt")

(define configuration #f)

#|
Config is a Racket module (site.rkt) that `(provide site)` a hash.

Old `.json` config is no longer a runtime format: seeing one is a one-time
migration trigger: we generate the equivalent site.rkt, then suggest deleting the .json,
and then load the freshly written .rkt so this very build keeps working.
|#
(define (setup-config! filepath)
  (define path-str (if (path? filepath) (path->string filepath) filepath))
  (define rkt-cfg-path (path-replace-extension filepath #".rkt"))
  (define rkt-path
    (cond
      [(path-has-extension? filepath #".rkt") filepath]
      [(and (path-has-extension? filepath #".json")
            (file-exists? rkt-cfg-path))
       (eprintf "tr: ~a is already existed, please use it instead (ignore ~a)\n" 
                rkt-cfg-path path-str)]
      [else
        (call-with-output-file rkt-cfg-path
          (lambda (out) (upgrade-json-config! (file->json filepath) out))
          (eprintf "tr: already produce ~a from ~a - please use ~a and delete ~a\n"
                   rkt-cfg-path path-str rkt-cfg-path path-str))
        rkt-cfg-path]))
  (set! configuration (dynamic-require (path->complete-path rkt-path) 'site)))

; Upgrade old JSON config as a site.rkt source module.
(define (upgrade-json-config! j out)
  (define (emit-value v)
    (if (or (string? v) (number? v) (boolean? v))
        (format "~s" v)
        (format "'~s" v))) ; arrays, null symbol, ...
  (fprintf out "#lang racket/base\n(require scribble/html)\n(provide site)\n\n")
  (fprintf out "(define site\n  (hash")
  (define first? #t)
  (for ([k (sort (hash-keys j) symbol<?)]
        #:unless (eq? k 'fedi))
    (fprintf out "~a'~a ~a" (if first? " " "\n        ") k (emit-value (hash-ref j k)))
    (set! first? #f))
  ; the legacy `fedi` key is expanded into the two head) elements it used to inject (rel=me link + fediverse:creator meta)
  (define fedi (hash-ref j 'fedi #f))
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

(define (get-build-mode)
  (get-config 'mode "release"))

(define (dev-mode?)
  (equal? "dev" (get-build-mode)))

; A stable string of the config that the per-card renderer bakes into output.
; This feeds the build signature so two output targets with different such
; config get distinct signatures and never share a content-store entry.
;
; Today the only config that reaches per-card HTML is `head` (see
; generate-index.rkt). We key on the rendered bytes (xml->string) rather than
; the element values, which print opaquely and unstably.
(define (render-config-tag)
  (format "~s" (map xml->string (get-config 'head '()))))
