#lang racket
#|
Canonical, content-addressed store of a card's build artifacts, keyed by its
build signature (see signature.rkt). Because the signature captures everything
a card's output depends on -- including the render-affecting config -- two
builds that would render byte-identical output share one store entry. A second
output target therefore *copies* a card's output instead of re-rendering it,
and reverting a card to an earlier state is a copy of the still-cached entry
rather than a re-render.

A store entry `<cache-root>/store/<sig>/` caches only the artifacts:
  embed.html  -- the rendered embed (dynamic-rerequire; also lives at
                 _tmp/<addr>.embed.html)
  out/        -- the card's output dir <output-path>/<addr>/ minus index.html,
                 i.e. the compiled svgs (latex/typst)
index.html is NOT cached: it is a cheap computation of the embed plus the
card's metadata (produce-index).

The store keys correctness across targets, but says nothing about whether a
*given* output target already holds the current output. Each output dir keeps
a per-card stamp `<output>/<addr>/.sig` naming the signature its on-disk output
was produced for. When that stamp matches, the per-target output work (svgs +
index.html) is already current and is skipped -- only the embed is refreshed
into _tmp, which transcluding parents may still read.
|#
(provide init-store!
         store-hit?
         save-to-store!
         restore-embed!
         restore-output!
         output-fresh?
         write-output-stamp!)
(require "config.rkt")

(define (store-root cache-root) (build-path cache-root "store"))
(define (sig-dir cache-root sig) (build-path (store-root cache-root) sig))

(define (embed-file addr) (build-path "_tmp" (string-append addr ".embed.html")))
(define (out-dir addr) (build-path (get-output-path) addr))
(define (stamp-file addr) (build-path (out-dir addr) ".sig"))
(define (index-file addr) (build-path (out-dir addr) "index.html"))

; non-content files in an output dir: regenerated/managed per target, never cached
(define (per-target-file? name) (or (equal? name "index.html") (equal? name ".sig")))

(define (init-store! cache-root)
  (make-directory* (store-root cache-root)))

(define (store-hit? cache-root sig)
  (directory-exists? (sig-dir cache-root sig)))

(define (copy-into! src dst)
  (when (or (file-exists? dst) (directory-exists? dst))
    (delete-directory/files dst))
  (cond
    [(directory-exists? src) (copy-directory/files src dst)]
    [(file-exists? src) (copy-file src dst)]))

(define (save-to-store! cache-root sig addr)
  (define final (sig-dir cache-root sig))
  (define wip (build-path (store-root cache-root) (string-append ".wip-" sig)))
  (when (directory-exists? wip) (delete-directory/files wip))
  (make-directory* wip)
  (copy-into! (embed-file addr) (build-path wip "embed.html"))
  ; cache the output dir except the per-target files (index.html, .sig), which
  ; are regenerated/managed per target -- kept out by skipping them on the way in
  (when (directory-exists? (out-dir addr))
    (make-directory* (build-path wip "out"))
    (for ([entry (directory-list (out-dir addr))]
          #:unless (per-target-file? (path->string entry)))
      (copy-into! (build-path (out-dir addr) entry)
                  (build-path wip "out" entry))))
  (when (directory-exists? final) (delete-directory/files final))
  (rename-file-or-directory wip final))

; Restore just the embed into _tmp. Always done on a store hit, regardless of
; the per-target stamp, because a transcluding parent may render and read it.
(define (restore-embed! cache-root sig addr)
  (copy-into! (build-path (sig-dir cache-root sig) "embed.html") (embed-file addr)))

; Restore the cached output (svgs) into this target's output dir.
(define (restore-output! cache-root sig addr)
  (define out (build-path (sig-dir cache-root sig) "out"))
  (when (directory-exists? out)
    (make-directory* (get-output-path))
    (copy-into! out (out-dir addr))))

; Does this target's on-disk output already correspond to sig? Requires the
; stamp to match, the index to be present, and every cached output file for this
; sig to still exist in the target -- so a deleted/interrupted svg (or index)
; re-materializes even with the stamp still around.
(define (output-fresh? cache-root sig addr)
  (and (file-exists? (index-file addr))
       (file-exists? (stamp-file addr))
       (string=? sig (call-with-input-file (stamp-file addr) port->string))
       (cached-outputs-present? cache-root sig addr)))

(define (cached-outputs-present? cache-root sig addr)
  (define out (build-path (sig-dir cache-root sig) "out"))
  (or (not (directory-exists? out))
      (for/and ([f (in-directory out)] #:unless (directory-exists? f))
        (file-exists?
          (build-path (out-dir addr)
                      (find-relative-path (path->complete-path out)
                                          (path->complete-path f)))))))

(define (write-output-stamp! addr sig)
  (make-directory* (out-dir addr))
  (call-with-output-file (stamp-file addr) #:exists 'truncate/replace
    (lambda (o) (write-string sig o))))
