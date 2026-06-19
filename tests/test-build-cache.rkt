#lang racket
;; End-to-end test of content-addressed cache invalidation in search-and-build.
;; Plain-text cards only (no @m math / no tikz/typst) so no external tools are
;; needed. A card counts as "rebuilt" when its embed.html is regenerated.
(require json
         "../build.rkt"
         "../private/config.rkt")

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
    (write-file! (build-path "site.json")
                 "{\"domain\":\"example.com\",\"title\":\"T\",\"description\":\"D\",\"output-path\":\"_build\"}")
    (setup-config! "site.json")))

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
    (define cfg (format "site-~a.json" out))
    (write-file! (build-path cfg)
                 (format "{\"domain\":\"example.com\",\"title\":\"T\",\"description\":\"D\",\"output-path\":~s}" out))
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

  (test-case "reformatting metadata.json (newer mtime, source untouched) does NOT rebuild"
    ;; the mtime-based check would falsely skip-or-rebuild on metadata mtime; the
    ;; hash ignores the filesystem clock entirely.
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}"))
    (build!)
    (parameterize ([current-directory proj])
      ;; rewrite the metadata file with reordered whitespace and a fresh mtime,
      ;; same content - exactly what an external formatter would do
      (define mp (build-path "_tmp" "a.metadata.json"))
      (define j (call-with-input-file mp read-json))
      (call-with-output-file mp #:exists 'replace
        (lambda (o) (write-json j o) (newline o) (newline o))))
    (check-equal? (build!) (set) "a formatter touching metadata.json must not trigger a rebuild"))

  (test-case "editing the scrbl rebuilds even when metadata.json is newer (the original bug)"
    (fresh-project!)
    (parameterize ([current-directory proj])
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha}"))
    (build!)
    (parameterize ([current-directory proj])
      ;; real edit to the source ...
      (write-file! (build-path "content" "post" "a.scrbl")
                   "@title{A}" "@date{2024-01-01}" "@p{alpha edited}")
      ;; ... while a formatter bumps metadata.json to be NEWER than the source
      (define mp (build-path "_tmp" "a.metadata.json"))
      (define j (call-with-input-file mp read-json))
      (call-with-output-file mp #:exists 'replace (lambda (o) (write-json j o))))
    (check-true (rebuilt? (build!) "a")
                "content change must rebuild despite newer metadata mtime"))

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
                "the missing index.html was regenerated")))
