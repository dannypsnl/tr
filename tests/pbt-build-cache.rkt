#lang racket
;; Property-based companion to test-build-cache.rkt.
;;
;; test-build-cache.rkt hand-enumerates specific dependency shapes (a mentions
;; b, sx transcludes sy, ...). The property here instead generates a random
;; DAG of cards (mentions/transcludes only point at earlier cards, so it's
;; acyclic by construction) plus a random single-card edit, then checks the
;; invariant all of those hand-written cases are really instances of:
;;
;;   rebuilt-set(edit) == expected-rebuild(graph, edited-id, edit-kind)
;;
;; where expected-rebuild is field-scoped, mirroring private/signature.rkt's
;; compute-signatures (see its header comment):
;;   - @mention{} is a BIDIRECTIONAL, one-hop title dependency: if A mentions
;;     B, A's signature folds B's title (the link text, card.rkt:221 `mention`
;;     via fetch-metadata) AND B's signature folds A's title too, because
;;     mentioning auto-populates the mentioned card's `backlinks` metadata
;;     (build.rkt:129-150) and every card's signature folds title/taxon of
;;     its context/references/backlinks/related/authors neighbors regardless
;;     of whether the card visually renders that section. Body-only edits
;;     don't propagate either direction -- only title/taxon are folded.
;;   - @transclude{} is TWO different dependencies at once: a RECURSIVE one
;;     forward (the transcluder's signature folds the transcluded card's FULL
;;     signature, so any change to the child propagates up and chains through
;;     further transcludes), *and* a one-hop title dependency backward, since
;;     transcluding also auto-populates the transcluded card's `context`
;;     metadata with the transcluder's addr (build.rkt:132-135) -- exactly
;;     parallel to mention's backlinks. So editing a parent's *title* also
;;     rebuilds its transcluded children, one hop.
;;   - Neither direction chains a title change past one hop, though: no
;;     node's *title* ever changes as a side effect of another node's edit,
;;     only its rendered content does -- and content isn't folded into a
;;     neighbor's signature, only title/taxon is.
(require racket/set
         rackcheck
         "../private/build.rkt"
         "../private/config.rkt")

;; dynamic-rerequire caches a module by resolved path for the process's
;; lifetime and only reloads on a newer mtime (1s resolution); a
;; property-based run does many trials in one process, so reusing one fixed
;; proj dir + fixed card ids across trials risks a later trial's build
;; silently reading an earlier trial's cached module. Each trial gets its own
;; proj dir instead -- see tests/test-build-cache.rkt's similar note about
;; not reusing an addr a prior test-case already loaded.
(define trial-count (box 0))
(define proj #f)

(define (write-file! path . lines)
  (define dir (path-only path))
  (when dir (make-directory* dir))
  (call-with-output-file path #:exists 'replace
    (lambda (out) (for ([l lines]) (displayln l out)))))

(define (fresh-project!)
  (set-box! trial-count (add1 (unbox trial-count)))
  (set! proj (build-path "/tmp" (format "tr-test-build-cache-pbt-~a" (unbox trial-count))))
  (when (directory-exists? proj) (delete-directory/files proj))
  (make-directory* proj)
  (parameterize ([current-directory proj])
    (make-directory* "_build")
    (write-file! (build-path "site.rkt")
                 "#lang racket/base"
                 "(provide site)"
                 "(define site (hash 'domain \"example.com\" 'title \"T\" 'description \"D\" 'output-path \"_build\"))")
    (setup-config! "site.rkt")))

(define (build!)
  (parameterize ([current-directory proj])
    (define text (with-output-to-string (lambda () (search-and-build "content"))))
    (list->set (regexp-match* #px"generate (\\S+)\\.embed\\.html" text #:match-select cadr))))

;; bump a file's mtime into the future instead of (sleep 1); dynamic-rerequire
;; only reloads a module when its source's mtime has advanced past what it
;; last saw, at 1s resolution -- see tests/test-build-cache.rkt's comments.
(define (touch-forward! path)
  (file-or-directory-modify-seconds
    path (+ 1 (file-or-directory-modify-seconds path))))

;; ---------------------------------------------------------------------------
;; A card is (id title mentions transcludes body): id = "c0" .. "c(n-1)" in
;; generation order; mentions/transcludes ⊆ earlier ids, so the dependency
;; graph is acyclic by construction -- no need to detect/reject cycles.

(define words '("alpha" "beta" "gamma" "delta" "epsilon"))

;; independently assign each earlier id a role: referenced via @mention{},
;; via @transclude{}, or not referenced at all.
(define (gen:edges earlier-ids)
  (if (set-empty? earlier-ids)
      (gen:const (cons '() '()))
      (gen:let ([roles (apply gen:tuple
                              (set-map earlier-ids
                                       (lambda (_) (gen:one-of '(none mention transclude)))))])
               (cons (for/list ([x earlier-ids] [r roles] #:when (eq? r 'mention)) x)
                     (for/list ([x earlier-ids] [r roles] #:when (eq? r 'transclude)) x)))))

(define (gen:card i earlier-ids)
  (gen:let ([title (gen:one-of words)]
            [edges (gen:edges earlier-ids)]
            [body (gen:list (gen:one-of words) #:max-length 3)])
           (list (format "c~a" i) title (car edges) (cdr edges) body)))

;; gen:card-graph : Gen<(listof (list id title (listof id) (listof id) (listof word)))>
(define (gen:card-graph #:max-cards [max-cards 5])
  (gen:bind
    (gen:integer-in 1 max-cards)
    (lambda (n)
      (let loop ([i 0]
                 [ids (set)]
                 [gens '()])
        (if (= i n)
            (apply gen:tuple (reverse gens))
            (loop (add1 i)
                  (set-add ids (format "c~a" i))
                  (cons (gen:card i ids) gens)))))))

;; ---------------------------------------------------------------------------
;; card graph -> on-disk scrbl

(define (card->scrbl-lines c)
  (match-define (list id title mentions transcludes body) c)
  (define body-words (if (null? body) '("x") body))
  (define refs (append (for/list ([m mentions]) (format "@mention{~a}" m))
                       (for/list ([t transcludes]) (format "@transclude{~a}" t))))
  (list (format "@title{~a}" title)
        "@date{2024-01-01}"
        (string-join (cons (format "@p{~a}" (string-join body-words " ")) refs) " ")))

(define (write-graph! graph)
  (for ([c graph])
    (apply write-file!
           (build-path proj "content" "post" (format "~a.scrbl" (first c)))
           (card->scrbl-lines c))))

;; deterministically-different edit: appending `marker` always changes the
;; string, so we never accidentally generate a "no-op" edit that the
;; content-hash store would (correctly) treat as unchanged.
(define (apply-edit c edit-kind marker)
  (match-define (list id title mentions transcludes body) c)
  (case edit-kind
    [(title) (list id (string-append title "-" marker) mentions transcludes body)]
    [(body) (list id title mentions transcludes (cons marker body))]))

;; ---------------------------------------------------------------------------
;; expected rebuild set: field-scoped closure (see header comment)

(define (referrers-via graph edge-of)
  (define h (make-hash))
  (for ([c graph])
    (define id (first c))
    (for ([dep (edge-of c)]) (hash-update! h dep (lambda (l) (cons id l)) '())))
  h)

(define (closure-from h seed-ids)
  (let loop ([frontier seed-ids] [seen (list->set seed-ids)])
    (define next (for*/list ([id frontier] [r (hash-ref h id '())] #:unless (set-member? seen r)) r))
    (if (null? next) seen (loop next (set-union seen (list->set next))))))

;; Both mention and transclude are symmetric for title purposes, not just
;; mention: "A mentions B" makes A fold B's title (the link text) and, via
;; B's auto-populated backlinks, makes B fold A's title too (build.rkt:136-142).
;; "A transcludes B" makes A fold B's title (as part of B's full signature,
;; below) and, via B's auto-populated context, makes B fold A's title too
;; (build.rkt:132-135) -- so a title edit is a one-hop dependency across
;; *either* edge kind, independent of the separate recursive full-content
;; dependency transclude also creates (see transclude-referrers below).
(define (neighbor-title-deps-of graph)
  (define h (make-hash))
  (define (add! a b) (hash-update! h a (lambda (l) (cons b l)) '()))
  (for ([c graph])
    (define id (first c))
    (for ([m (append (third c) (fourth c))]) (add! id m) (add! m id)))
  h)

(define (expected-rebuild graph edited-id edit-kind)
  (define neighbor-title-deps (neighbor-title-deps-of graph))
  (define transclude-referrers (referrers-via graph fourth))
  (define seed (cons edited-id (if (eq? edit-kind 'title) (hash-ref neighbor-title-deps edited-id '()) '())))
  (closure-from transclude-referrers seed))

;; ---------------------------------------------------------------------------
;; the property

(define (single-edit-holds? graph edit-index edit-kind marker)
  (fresh-project!)
  (write-graph! graph)
  (build!) ; establish the store; first build always rebuilds everything

  (define edited-card (list-ref graph edit-index))
  (define edited-id (first edited-card))
  (define edited-path (build-path proj "content" "post" (format "~a.scrbl" edited-id)))
  (define graph* (list-set graph edit-index (apply-edit edited-card edit-kind marker)))

  (write-graph! (list (list-ref graph* edit-index)))
  (touch-forward! edited-path)

  (equal? (build!) (expected-rebuild graph edited-id edit-kind)))

(define prop:single-edit-invalidation
  (property single-edit-invalidation
            ([graph (gen:card-graph #:max-cards 5)]
             [edit-index (gen:integer-in 0 (sub1 (length graph)))]
             [edit-kind (gen:one-of '(title body))]
             [marker (gen:one-of words)])
            (single-edit-holds? graph edit-index edit-kind marker)))

(module+ test
  (require rackunit)

  ;; hand-picked instances pinning down the propagation rules before
  ;; trusting the generator/shrinker with them.
  (test-case "mention is bidirectional on title edit, not on body edit"
    (define graph (list (list "c0" "Zero" '() '() '("alpha"))
                        (list "c1" "One" '("c0") '() '("beta"))))
    (check-true (single-edit-holds? graph 0 'title "x")
                "c1 mentions c0: c0's title changed, c1 must be in the rebuild set")
    (check-true (single-edit-holds? graph 0 'body "y")
                "c1 mentions c0: c0's body-only change must NOT drag c1 in")
    (check-true (single-edit-holds? graph 1 'title "x")
                "c1's own title changed: c0 must also rebuild via c0's backlinks")
    (check-true (single-edit-holds? graph 1 'body "y")
                "c1's body-only change must NOT drag c0 in (backlinks fold title only)"))

  (test-case "transclude referrer rebuilds on any edit, and it chains"
    (define graph (list (list "c0" "Zero" '() '() '("alpha"))
                        (list "c1" "One" '() '("c0") '("beta"))
                        (list "c2" "Two" '() '("c1") '("gamma"))))
    (check-true (single-edit-holds? graph 0 'body "y")
                "c1 transcludes c0 and c2 transcludes c1: a body-only edit to c0 must rebuild all three"))

  (test-case "transclude is also bidirectional on title edit (via context, not just backlinks)"
    (define graph (list (list "c0" "delta" '() '() '())
                        (list "c1" "beta" '() '() '())
                        (list "c2" "gamma" '() '("c1") '())
                        (list "c3" "delta" '("c1") '("c0") '())))
    (check-true (single-edit-holds? graph 2 'title "epsilon")
                "c2 transcludes c1: c2's title changed, c1 must also rebuild via c1's context"))

  ;; kept low by default -- each trial does two real builds (real
  ;; dynamic-rerequire + filesystem IO), not just pure computation. A
  ;; scheduled CI job can afford far more trials than push/PR CI; override
  ;; via TR_PBT_TESTS (see .github/workflows/pbt-nightly.yml).
  (define num-trials
    (or (let ([v (getenv "TR_PBT_TESTS")]) (and v (string->number v)))
        40))
  ;; make-config's own default deadline is a fixed 60s regardless of #:tests,
  ;; so a large TR_PBT_TESTS run would silently truncate ("timed out") well
  ;; before finishing; scale it with num-trials instead (generous per-trial
  ;; ceiling -- this only bounds worst case, it doesn't slow a normal run).
  (check-property (make-config #:tests num-trials
                               #:deadline (+ (current-inexact-milliseconds) (* num-trials 5000)))
                  prop:single-edit-invalidation))
