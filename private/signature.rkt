#lang racket
(provide scrbl-include-paths
         compute-source-hash
         compute-signatures)
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
                           (if (file-exists? full) (file->bytes full) #"")))))
    
  (sha1 (open-input-bytes (apply bytes-append (frame scrbl) included))))

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
; - the title/taxon of every other referenced neighbor -- the only fields it
;   renders (context/references/backlinks/related/authors). Headings are not
;   source hash, so editing a parent never rebuilds its transcluded children
; - config-tag: the render-affecting site config (see render-config-tag). The
;   signature keys the canonical content store, which is shared across output
;   targets; folding config in means two targets whose rendered bytes would
;   differ (e.g. different `fedi`) get distinct signatures and never copy each
;   other's output. Targets that differ only in where files land share the store.
(define (compute-signatures topo-addrs addr->path addr->meta tmp-dir config-tag)
  (define src-cache (make-hash))
  (define (source-hash-of addr)
    (hash-ref! src-cache addr
               (lambda ()
                 (define p (hash-ref addr->path addr #f))
                 (if p (compute-source-hash p tmp-dir) ""))))
  (define (neighbor-heading addr)
    (define m (hash-ref addr->meta addr #f))
    (if m
        (format "~a/~a" (hash-ref m 'title "") (hash-ref m 'taxon ""))
        ""))
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
      (sort (for/list ([n neighbors]) (format "~a=~a" n (neighbor-heading n))) string<?))
    (hash-set! sigs addr
               (sha1 (open-input-bytes
                       (apply bytes-append
                              (frame-str config-tag)
                              (frame-str (source-hash-of addr))
                              (frame-str (canonical meta))
                              (append (map frame-str child-sigs)
                                      (map frame-str neighbor-digests)))))))
  sigs)
