#lang racket
(require "../private/signature.rkt")

(define temp-dir "/tmp/tr-test-signature")

(define (reset!)
  (when (directory-exists? temp-dir)
    (delete-directory/files temp-dir))
  (make-directory* (build-path temp-dir "_tmp")))

(define (write-file! path . lines)
  (call-with-output-file path #:exists 'replace
    (lambda (out) (for ([l lines]) (displayln l out)))))

(module+ test
  (require rackunit)

  ;; --- scrbl-include-paths ---
  (test-case "scrbl-include-paths finds top-level and nested @include forms"
    (reset!)
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src
                 "@title{A}"
                 "@include{html/a.html}"
                 "@p{body @include{html/b.html}}")
    (check-equal? (list->set (scrbl-include-paths src))
                  (set "html/a.html" "html/b.html")))

  (test-case "scrbl-include-paths returns empty when no includes"
    (reset!)
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src "@title{A}" "@p{just text}")
    (check-equal? (scrbl-include-paths src) '()))

  ;; --- compute-source-hash ---
  (define tmp (build-path temp-dir "_tmp"))

  (test-case "compute-source-hash is deterministic and changes with scrbl content"
    (reset!)
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src "@title{A}" "@p{hello}")
    (define h1 (compute-source-hash src tmp))
    (check-equal? h1 (compute-source-hash src tmp) "same inputs -> same hash")
    (write-file! src "@title{A}" "@p{goodbye}")
    (check-not-equal? h1 (compute-source-hash src tmp) "edited scrbl -> new hash"))

  (test-case "compute-source-hash changes when an @included file changes"
    (reset!)
    (make-directory* (build-path tmp "html"))
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src "@title{A}" "@include{html/a.html}")
    (define inc (build-path tmp "html" "a.html"))
    (write-file! inc "AGDA ONE")
    (define h1 (compute-source-hash src tmp))
    (write-file! inc "AGDA TWO")
    (check-not-equal? h1 (compute-source-hash src tmp)
                      "rewriting the @included html (scrbl untouched) -> new hash"))

  (test-case "compute-source-hash resolves include paths relative to tmp-dir"
    (reset!)
    (make-directory* (build-path tmp "html"))
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src "@title{A}" "@include{html/a.html}")
    ; a same-named file outside tmp must NOT affect the hash
    (make-directory* (build-path temp-dir "html"))
    (write-file! (build-path temp-dir "html" "a.html") "DECOY")
    (write-file! (build-path tmp "html" "a.html") "REAL")
    (define h1 (compute-source-hash src tmp))
    (write-file! (build-path temp-dir "html" "a.html") "DECOY CHANGED")
    (check-equal? h1 (compute-source-hash src tmp)
                  "file outside tmp-dir is irrelevant")
    (write-file! (build-path tmp "html" "a.html") "REAL CHANGED")
    (check-not-equal? h1 (compute-source-hash src tmp)
                      "file under tmp-dir is the one that counts"))

  (test-case "compute-source-hash tolerates a missing included file"
    (reset!)
    (define src (build-path temp-dir "a.scrbl"))
    (write-file! src "@title{A}" "@include{html/missing.html}")
    (check-true (string? (compute-source-hash src tmp))))

  ;; --- compute-signatures ---
  ;; helpers to build a metadata hash and a backing .scrbl per addr
  (define (meta addr
                #:transclude [transclude '()]
                #:related [related '()]
                #:context [context '()]
                #:references [references '()]
                #:backlinks [backlinks '()]
                #:authors [authors '()])
    (hash 'id addr 'title (list addr) 'taxon "Note"
          'transclude transclude 'related related 'context context
          'references references 'backlinks backlinks 'authors authors))

  (define (scrbl! addr . lines)
    (define p (build-path temp-dir (string-append addr ".scrbl")))
    (apply write-file! p lines)
    p)

  (test-case "signature changes when the card's own source changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@title{A}" "@p{one}")))
    (define a->m (hash "a" (meta "a")))
    (define s1 (hash-ref (compute-signatures '("a") a->p a->m tmp "cfg") "a"))
    (scrbl! "a" "@title{A}" "@p{two}")
    (define s2 (hash-ref (compute-signatures '("a") a->p a->m tmp "cfg") "a"))
    (check-not-equal? s1 s2))

  (test-case "signature changes when a transcluded child's content changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@transclude{b}")
                       "b" (scrbl! "b" "@title{B}" "@p{child one}")))
    (define a->m (hash "a" (meta "a" #:transclude '("b"))
                       "b" (meta "b")))
    ; topo order: child b before parent a
    (define s1 (hash-ref (compute-signatures '("b" "a") a->p a->m tmp "cfg") "a"))
    (scrbl! "b" "@title{B}" "@p{child two}")
    (define s2 (hash-ref (compute-signatures '("b" "a") a->p a->m tmp "cfg") "a"))
    (check-not-equal? s1 s2 "parent rebuilds when embedded child changes"))

  (test-case "signature changes when a non-transclude neighbor's title changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "b" (scrbl! "b" "@title{B title}")))
    ; a renders b's title via its 'related list, so a depends on b's title/taxon
    (define (m b-title) (hash "a" (meta "a" #:related '("b"))
                              "b" (hash-set (meta "b") 'title b-title)))
    (define s1 (hash-ref (compute-signatures '("a" "b") a->p (m '("B title")) tmp "cfg") "a"))
    (define s2 (hash-ref (compute-signatures '("a" "b") a->p (m '("B new title")) tmp "cfg") "a"))
    (check-not-equal? s1 s2 "neighbor display change invalidates referrer"))

  (test-case "signature is stable when a non-transclude neighbor's body changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "b" (scrbl! "b" "@title{B}" "@p{body one}")))
    ; a only renders b's title/taxon, so editing b's body must NOT rebuild a
    (define a->m (hash "a" (meta "a" #:related '("b"))
                       "b" (meta "b")))
    (define s1 (hash-ref (compute-signatures '("a" "b") a->p a->m tmp "cfg") "a"))
    (scrbl! "b" "@title{B}" "@p{body two}")
    (define s2 (hash-ref (compute-signatures '("a" "b") a->p a->m tmp "cfg") "a"))
    (check-equal? s1 s2 "neighbor body edit does not invalidate referrer"))

  (test-case "signature is stable when an unrelated card changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "c" (scrbl! "c" "@p{c}")))
    (define a->m (hash "a" (meta "a") "c" (meta "c")))
    (define s1 (hash-ref (compute-signatures '("a" "c") a->p a->m tmp "cfg") "a"))
    (scrbl! "c" "@p{c changed}")
    (define s2 (hash-ref (compute-signatures '("a" "c") a->p a->m tmp "cfg") "a"))
    (check-equal? s1 s2 "unrelated edits do not invalidate a"))

  (test-case "signature changes when the card's own metadata changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")))
    (define s1 (hash-ref (compute-signatures '("a") a->p (hash "a" (meta "a")) tmp "cfg") "a"))
    (define s2 (hash-ref (compute-signatures '("a") a->p
                                             (hash "a" (meta "a" #:backlinks '("z"))) tmp "cfg") "a"))
    (check-not-equal? s1 s2 "a new incoming backlink invalidates the card"))

  (test-case "mutual references (cycle) terminate"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "b" (scrbl! "b" "@p{b}")))
    ; a relates b, b relates a -> would cycle if digests folded full sigs
    (define a->m (hash "a" (meta "a" #:related '("b") #:backlinks '("b"))
                       "b" (meta "b" #:related '("a") #:backlinks '("a"))))
    (define sigs (compute-signatures '("a" "b") a->p a->m tmp "cfg"))
    (check-true (string? (hash-ref sigs "a")))
    (check-true (string? (hash-ref sigs "b"))))

  (test-case "signature changes when the render-affecting config changes"
    (reset!)
    ; identical content + identical output target, but a different config-tag
    ; (e.g. a different `fedi`) bakes different bytes into the per-card HTML, so
    ; the two must not share a content-store entry
    (define a->p (hash "a" (scrbl! "a" "@p{a}")))
    (define a->m (hash "a" (meta "a")))
    (define s1 (hash-ref (compute-signatures '("a") a->p a->m tmp "cfg-one") "a"))
    (define s2 (hash-ref (compute-signatures '("a") a->p a->m tmp "cfg-two") "a"))
    (check-not-equal? s1 s2 "render-affecting config change yields a distinct signature")))
