#lang racket
(provide scrbl-include-paths
         compute-source-hash
         compute-signatures
         read-cache-map
         write-cache-marker!
         clear-addr-markers!
         cache-hit?)
(require scribble/reader
         file/sha1
         "common.rkt")

; Walk the scribble forms of a .scrbl source and collect every path referenced
; by a `@include{path}` (scribble/text include), including nested occurrences.
; These extra inputs feed into the source hash so external HTMLs change invalidate
; the card even when the .scrbl bytes are untouched.
(define (scrbl-include-paths source-path)
  (define forms
    (call-with-input-file source-path
      (lambda (in) (read-inside in))))
  (define paths (box '()))
  (let walk ([form forms])
    (match form
      [`(include ,(? string? p)) (set-box! paths (cons p (unbox paths)))]
      [(? list?) (for-each walk form)]
      [_ (void)]))
  (reverse (unbox paths)))

; Length-framed chunk so distinct chunk boundaries won't collide
; (e.g. "ab"+"c" vs "a"+"bc").
(define (frame bs)
  (bytes-append (string->bytes/utf-8 (number->string (bytes-length bs))) #":" bs))

; Hash of everything a card's rendered body depends on in its own source:
; the .scrbl bytes plus the bytes of every file it @includes. Include paths are
; resolved against tmp-dir, which is where the generated embed scrbl lives and
; therefore where scribble/text's `include` resolves them at render time.
(define (compute-source-hash source-path tmp-dir)
  (define scrbl (file->bytes source-path))
  (define included
    (for/list ([p (scrbl-include-paths source-path)])
      (define full (build-path tmp-dir p))
      (frame (bytes-append (string->bytes/utf-8 p) #"\0"
                           (if (file-exists? full) (file->bytes full) #""))))
    )
  (sha1 (open-input-bytes (apply bytes-append (frame scrbl) included))))

; A cache marker is an empty file named "<addr>:<sig>". Top-level addrs never
; contain ":" (that is exactly what `non-local?` keys on), so the split is
; unambiguous. Reading the directory once yields the full set of built
; (addr, sig) pairs; a cache hit is pure presence-testing, no file reads.
(define (marker-name addr sig) (format "~a:~a" addr sig))

(define (read-cache-map cache-dir)
  (cond
    [(directory-exists? cache-dir)
     (list->set (map path->string (directory-list cache-dir)))]
    [else (set)]))

(define (cache-hit? cache-map addr sig)
  (set-member? cache-map (marker-name addr sig)))

(define (write-cache-marker! cache-dir addr sig)
  (close-output-port
    (open-output-file (build-path cache-dir (marker-name addr sig))
                      #:exists 'truncate/replace)))

(define (marker-addr name)
  (define i (for/first ([c (in-string name)] [k (in-naturals)] #:when (char=? c #\:)) k))
  (and i (substring name 0 i)))

(define (clear-addr-markers! cache-dir addr)
  (when (directory-exists? cache-dir)
    (for ([entry (directory-list cache-dir)])
      (define name (path->string entry))
      (when (equal? (marker-addr name) addr)
        (delete-file (build-path cache-dir entry))))))

; Deterministic textual encoding of a jsexpr: hash keys are sorted so the
; serialization (and thus the signature) never depends on hash iteration order.
(define (canonical v)
  (cond
    [(hash? v)
     (string-append
       "{" (string-join (for/list ([k (sort (hash-keys v) symbol<?)])
                          (format "~a=~a" k (canonical (hash-ref v k)))) ",") "}")]
    [(list? v) (string-append "[" (string-join (map canonical v) ",") "]")]
    [else (format "~s" v)]))

; The build signature of a card captures everything its rendered output depends
; on, computed in transclude-topological order (children before parents):
; - its own source hash (.scrbl + @included files)
; - its own computed metadata (own fields + propagated backlinks/context/...)
; - the full signature of each transcluded child (its embedded output, and,
;   recursively, the transitive TOC titles) -- a DAG, so this is well-founded
; - the source-hash *digest* of every other referenced neighbor whose title/
;   taxon it renders (context/references/backlinks/related/authors). Digests
;   are folded instead of signatures so mutual references cannot cycle.
(define (compute-signatures topo-addrs addr->path addr->meta tmp-dir)
  (define src-cache (make-hash))
  (define (source-hash-of addr)
    (hash-ref! src-cache addr
               (lambda ()
                 (define p (hash-ref addr->path addr #f))
                 (if p (compute-source-hash p tmp-dir) ""))))
  (define (frame-str s) (frame (string->bytes/utf-8 s)))
  (define sigs (make-hash))
  (for ([addr topo-addrs])
    (define meta (hash-ref addr->meta addr))
    (define children
      (filter non-local? (hash-ref meta 'transclude '())))
    (define neighbors
      (remove-duplicates
        (filter non-local?
                (append (hash-ref meta 'context '())
                        (hash-ref meta 'references '())
                        (hash-ref meta 'backlinks '())
                        (hash-ref meta 'related '())
                        (hash-ref meta 'authors '())))))
    (define child-sigs
      (sort (for/list ([c children]) (hash-ref sigs c "")) string<?))
    (define neighbor-digests
      (sort (for/list ([n neighbors]) (format "~a=~a" n (source-hash-of n))) string<?))
    (hash-set! sigs addr
               (sha1 (open-input-bytes
                       (apply bytes-append
                              (frame-str (source-hash-of addr))
                              (frame-str (canonical meta))
                              (append (map frame-str child-sigs)
                                      (map frame-str neighbor-digests)))))))
  sigs)
