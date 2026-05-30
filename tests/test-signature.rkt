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

  ;; --- cache markers ---
  (test-case "write/read cache markers and cache-hit?"
    (reset!)
    (define cache (build-path tmp "cache"))
    (make-directory* cache)
    (write-cache-marker! cache "foo" "deadbeef")
    (write-cache-marker! cache "bar" "cafe")
    (define m (read-cache-map cache))
    (check-true (cache-hit? m "foo" "deadbeef"))
    (check-true (cache-hit? m "bar" "cafe"))
    (check-false (cache-hit? m "foo" "cafe") "wrong sig -> miss")
    (check-false (cache-hit? m "baz" "deadbeef") "unknown addr -> miss"))

  (test-case "clear-addr-markers! removes only the named addr's markers"
    (reset!)
    (define cache (build-path tmp "cache"))
    (make-directory* cache)
    (write-cache-marker! cache "foo" "oldsig")
    (write-cache-marker! cache "foobar" "keepme")
    (clear-addr-markers! cache "foo")
    (write-cache-marker! cache "foo" "newsig")
    (define m (read-cache-map cache))
    (check-false (cache-hit? m "foo" "oldsig") "stale marker gone")
    (check-true (cache-hit? m "foo" "newsig") "fresh marker present")
    (check-true (cache-hit? m "foobar" "keepme") "prefix-similar addr untouched"))

  (test-case "read-cache-map on empty/fresh cache dir is a miss for everything"
    (reset!)
    (define cache (build-path tmp "cache"))
    (make-directory* cache)
    (define m (read-cache-map cache))
    (check-false (cache-hit? m "anything" "whatever")))

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
    (define s1 (hash-ref (compute-signatures '("a") a->p a->m tmp) "a"))
    (scrbl! "a" "@title{A}" "@p{two}")
    (define s2 (hash-ref (compute-signatures '("a") a->p a->m tmp) "a"))
    (check-not-equal? s1 s2))

  (test-case "signature changes when a transcluded child's content changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@transclude{b}")
                       "b" (scrbl! "b" "@title{B}" "@p{child one}")))
    (define a->m (hash "a" (meta "a" #:transclude '("b"))
                       "b" (meta "b")))
    ; topo order: child b before parent a
    (define s1 (hash-ref (compute-signatures '("b" "a") a->p a->m tmp) "a"))
    (scrbl! "b" "@title{B}" "@p{child two}")
    (define s2 (hash-ref (compute-signatures '("b" "a") a->p a->m tmp) "a"))
    (check-not-equal? s1 s2 "parent rebuilds when embedded child changes"))

  (test-case "signature changes when a non-transclude neighbor's source changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "b" (scrbl! "b" "@title{B title}")))
    ; a renders b's title via its 'related list (footer-common)
    (define a->m (hash "a" (meta "a" #:related '("b"))
                       "b" (meta "b")))
    (define s1 (hash-ref (compute-signatures '("a" "b") a->p a->m tmp) "a"))
    (scrbl! "b" "@title{B new title}")
    (define s2 (hash-ref (compute-signatures '("a" "b") a->p a->m tmp) "a"))
    (check-not-equal? s1 s2 "neighbor display change invalidates referrer"))

  (test-case "signature is stable when an unrelated card changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "c" (scrbl! "c" "@p{c}")))
    (define a->m (hash "a" (meta "a") "c" (meta "c")))
    (define s1 (hash-ref (compute-signatures '("a" "c") a->p a->m tmp) "a"))
    (scrbl! "c" "@p{c changed}")
    (define s2 (hash-ref (compute-signatures '("a" "c") a->p a->m tmp) "a"))
    (check-equal? s1 s2 "unrelated edits do not invalidate a"))

  (test-case "signature changes when the card's own metadata changes"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")))
    (define s1 (hash-ref (compute-signatures '("a") a->p (hash "a" (meta "a")) tmp) "a"))
    (define s2 (hash-ref (compute-signatures '("a") a->p
                                             (hash "a" (meta "a" #:backlinks '("z"))) tmp) "a"))
    (check-not-equal? s1 s2 "a new incoming backlink invalidates the card"))

  (test-case "mutual references (cycle) terminate"
    (reset!)
    (define a->p (hash "a" (scrbl! "a" "@p{a}")
                       "b" (scrbl! "b" "@p{b}")))
    ; a relates b, b relates a -> would cycle if digests folded full sigs
    (define a->m (hash "a" (meta "a" #:related '("b") #:backlinks '("b"))
                       "b" (meta "b" #:related '("a") #:backlinks '("a"))))
    (define sigs (compute-signatures '("a" "b") a->p a->m tmp))
    (check-true (string? (hash-ref sigs "a")))
    (check-true (string? (hash-ref sigs "b")))))
