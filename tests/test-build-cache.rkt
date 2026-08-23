#lang racket
;; end-to-end test of content-addressed cache invalidation in search-and-build
(require "../private/build.rkt"
         "../private/config.rkt"
         "../private/metadata-store.rkt")

(define proj "/tmp/tr-test-build-cache")

(define (write-file! path . lines)
  (define dir (path-only path))
  (when dir (make-directory* dir))
  (call-with-output-file path #:exists 'replace
    (lambda (out) (for ([l lines]) (displayln l out)))))

(define (fresh-project!)
  (when (directory-exists? proj) (delete-directory/files proj))
  (make-directory* proj)
  (parameterize ([current-directory proj])
    (make-directory* "_build")
    (write-file! (build-path "site.rkt")
                 "#lang racket/base"
                 "(provide site)"
                 "(define site (hash 'domain \"example.com\" 'title \"T\" 'description \"D\" 'output-path \"_build\"))")
    (setup-config! "site.rkt")))

;; run a build, return the set of addrs whose embed.html was (re)generated
(define (build!)
  (parameterize ([current-directory proj])
    (define text (with-output-to-string (lambda () (search-and-build "content"))))
    (list->set (regexp-match* #px"generate (\\S+)\\.embed\\.html" text #:match-select cadr))))

(define (rebuilt? built addr) (set-member? built addr))

;; build the same content into a different output dir (its own site config),
;; returning the set of (re)rendered addrs; used to exercise the cross-target
;; content store
(define (build-into! out)
  (parameterize ([current-directory proj])
    (define cfg (format "site-~a.rkt" out))
    (write-file! (build-path cfg)
                 "#lang racket/base"
                 "(provide site)"
                 (format "(define site (hash 'domain \"example.com\" 'title \"T\" 'description \"D\" 'output-path ~s))" out))
    (setup-config! cfg))
  (build!))

;; the rendered <output>/<addr>/index.html as a string
(define (output-index out addr)
  (file->string (build-path proj out addr "index.html")))

(module+ test
  (require rackunit)

  (test-case "first build renders every card; an unchanged rebuild renders none"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha} @mention{b}")
      (write-file! (build-path "content" "post" "b.scrbl")
                   "@title{B}" "@date{2024-01-02}" "@p{beta}"))
    (define first (build!))
    (check-true (rebuilt? first "a"))
    (check-true (rebuilt? first "b"))
    (define second (build!))
    (check-equal? second (set) "nothing rebuilds when nothing changed"))

  (test-case "rewriting the metadata store row (content unchanged) does NOT rebuild"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}"))
    (build!)
    (parameterize ([current-directory proj])
      (open-metadata-store!)
      (metadata-store-set! "a" (metadata-store-ref "a"))
      (close-metadata-store!))
    (check-equal? (build!) (set) "rewriting the store row with identical content must not trigger a rebuild"))

  (test-case "editing the scrbl rebuilds even when the metadata store row was touched after"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}"))
    (build!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha edited}")
      (open-metadata-store!)
      (metadata-store-set! "a" (metadata-store-ref "a"))
      (close-metadata-store!))
    (check-true (rebuilt? (build!) "a")
                "content change must rebuild despite the store row being touched"))

  (test-case "changing an @included file rebuilds the card with the scrbl untouched"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}" "@include{html/a.html}")
      (make-directory* (build-path "_tmp" "html"))
      (write-file! (build-path "_tmp" "html" "a.html") "<div>agda one</div>"))
    (build!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "_tmp" "html" "a.html") "<div>agda two</div>"))
    (check-true (rebuilt? (build!) "a")
                "regenerated Agda html invalidates the card (no Makefile rm -f needed)"))

  (test-case "changing a @tr/depends file rebuilds the card with the scrbl untouched"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}" "@tr/depends{assets/anim.js}")
      (write-file! (build-path "assets" "anim.js") "console.log('one')"))
    (build!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "assets" "anim.js") "console.log('two')"))
    (check-true (rebuilt? (build!) "a")
                "editing the hand-authored asset invalidates the card even though it is never spliced into the scrbl"))

  (test-case "editing a mentioned neighbor's title rebuilds the referrer"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha} @mention{b}")
      (write-file! (build-path "content" "post" "b.scrbl")
                   "@title{B}" "@date{2024-01-02}" "@p{beta}"))
    (build!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "b.scrbl")
                   "@title{B renamed}" "@date{2024-01-02}" "@p{beta}"))
    (define built (build!))
    (check-true (rebuilt? built "b") "b itself changed")
    (check-true (rebuilt? built "a")
                "a renders b's title in its Related section, so a must rebuild"))

  ;; NOTE: these two cases assert on rendered output *content*, so they use addrs
  ;; (sx/sy, rv) not built by any earlier case. produce-html renders an embed via
  ;; dynamic-rerequire, which caches a module by path for the lifetime of the
  ;; process and only re-instantiates it on a fresh load; reusing an addr a prior
  ;; case already loaded would yield an empty re-render here.

  (test-case "a second output target copies from the store instead of re-rendering"
    ;; The deploy path builds two trees (e.g. dev then release) from one shared
    ;; _tmp. Identical content + identical render-config means identical
    ;; signatures, so the second target is a pure copy out of the content store:
    ;; nothing re-renders, yet its output is present and byte-correct.
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "sx.scrbl")
                   "@title{SX}" "@date{2024-01-01}" "@p{alpha} @transclude{sy}")
      (write-file! (build-path "content" "post" "sy.scrbl")
                   "@title{SY}" "@date{2024-01-02}" "@p{beta one}"))
    (build-into! "_out1")
    (define out2 (build-into! "_out2"))
    (check-equal? out2 (set) "second target renders nothing; it copies from the store")
    (check-true (regexp-match? #rx"beta one" (output-index "_out2" "sy"))
                "_out2/sy/index.html was materialized from the store")
    (check-true (regexp-match? #rx"beta one" (output-index "_out2" "sx"))
                "_out2/sx/index.html (which transcludes sy) was materialized too"))

  (test-case "reverting a card materializes the earlier output from the store"
    ;; A content store makes a revert a cache HIT: the earlier signature's entry
    ;; is still on disk, so the on-disk output is restored to the earlier render
    ;; rather than being left at the intermediate one.
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "rv.scrbl")
                   "@title{RV}" "@date{2024-01-01}" "@p{version one}"))
    (build!)
    ;; produce-html re-renders via dynamic-rerequire, which only reloads a module
    ;; when its source's modify-SECONDS advanced; a same-second rewrite would not
    ;; re-instantiate, so wait out the 1s resolution before the real edit.
    (sleep 1)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "rv.scrbl")
                   "@title{RV}" "@date{2024-01-01}" "@p{version two}"))
    (build!)
    (check-true (regexp-match? #rx"version two" (output-index "_build" "rv")))
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "rv.scrbl")
                   "@title{RV}" "@date{2024-01-01}" "@p{version one}"))
    (define built (build!))
    (check-equal? built (set) "revert hits the still-cached earlier entry, no re-render")
    (check-true (regexp-match? #rx"version one" (output-index "_build" "rv"))
                "on-disk output reverted to version one, not left at version two"))

  (test-case "an unchanged rebuild does not rewrite index.html (per-target stamp)"
    ;; the .sig stamp records which signature this target's output was built for;
    ;; when it still matches, produce-index! is skipped, so index.html is left
    ;; untouched on disk (its mtime does not advance).
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "st.scrbl")
                   "@title{ST}" "@date{2024-01-01}" "@p{stamped}"))
    (build!)
    (define idx (build-path proj "_build" "st" "index.html"))
    (check-true (file-exists? (build-path proj "_build" "st" ".sig"))
                "a per-card stamp is written next to the output")
    (define mtime0 (file-or-directory-modify-seconds idx))
    (sleep 1) ; modify-seconds has 1s resolution; advance the clock past it
    (build!)
    (check-equal? (file-or-directory-modify-seconds idx) mtime0
                  "unchanged rebuild leaves index.html untouched (produce-index! skipped)"))

  (test-case "a deleted output re-materializes despite a matching stamp"
    ;; output-fresh? does not trust the stamp alone: if a cached output file is
    ;; missing (here index.html, deleted to simulate an interrupted/tampered
    ;; build) the next build rebuilds the output from the still-cached entry.
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "hl.scrbl")
                   "@title{HL}" "@date{2024-01-01}" "@p{healed}"))
    (build!)
    (define idx (build-path proj "_build" "hl" "index.html"))
    (delete-file idx)
    (define built (build!))
    (check-equal? built (set) "still a store hit -- no re-render")
    (check-true (and (file-exists? idx) (regexp-match? #rx"healed" (output-index "_build" "hl")))
                "the missing index.html was regenerated"))

  (test-case "site.rkt's extension-module is auto-required into every card, and editing it invalidates the store"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "macro.rkt")
                   "#lang racket/base"
                   "(provide hi)"
                   "(define (hi x) (format \"hello-~a\" x))")
      ; a distinct filename from "site.rkt": dynamic-require caches by resolved
      ; path, so re-requiring the same "site.rkt" fresh-project! already loaded
      ; would silently return its stale (no-extension-module) config.
      (write-file! (build-path "site-em.rkt")
                   "#lang racket/base"
                   "(provide site)"
                   (string-append "(define site (hash 'domain \"example.com\" 'title \"T\" 'description \"D\""
                                  " 'output-path \"_build\" 'extension-module \"macro.rkt\"))"))
      (setup-config! "site-em.rkt")
      (write-file! (build-path "content" "post" "em.scrbl")
                   "@title{EM}" "@date{2024-01-01}" "@p{@hi{world}}"))
    (check-true (rebuilt? (build!) "em"))
    (check-true (regexp-match? #rx"hello-world" (output-index "_build" "em"))
                "the extension-module's binding is callable from the card with no per-card require")

    (sleep 1) ; dynamic-rerequire's mtime check has 1s resolution; see the revert test above
    (parameterize ([current-directory proj])
      (write-file! (build-path "macro.rkt")
                   "#lang racket/base"
                   "(provide hi)"
                   "(define (hi x) (format \"goodbye-~a\" x))"))
    (check-true (rebuilt? (build!) "em")
                "editing the extension-module (scrbl untouched) must invalidate the store")
    (check-true (regexp-match? #rx"goodbye-world" (output-index "_build" "em"))
                "the rebuild picks up the extension-module's new definition")))
