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
                "a renders b's title in its Related section, so a must rebuild")))
