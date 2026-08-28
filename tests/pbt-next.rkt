#lang racket
;; A small space-size (not the default space 36^4) keeps each trial's
;; O(space-size) setup and worst-case fallback scan cheap, so hundreds of
;; trials run in well under a second.
(require racket/set
         rackcheck
         "../private/next.rkt")

;; Splits [0, space-size) into `free-count` free positions and the rest
;; used, so every trial's free/used ratio is exactly controlled (free-count
;; = 0 is "exhausted", free-count = space-size is "untouched") instead of
;; left to chance the way generating a random used-list directly would be.
(define (free/used-split space-size free-count)
  (define positions (shuffle (range space-size)))
  (define free-set (list->set (take positions free-count)))
  (values free-set
          (filter (lambda (n) (not (set-member? free-set n))) (range space-size))))

(define (sound? space-size free-count)
  (define-values (free-set used-numbers) (free/used-split space-size free-count))
  (cond
    [(zero? free-count)
     (with-handlers ([exn:fail? (lambda (_) #t)])
       (random-unused-address used-numbers #:space-size space-size)
       #f)] ; should have raised instead of returning
    [else
     (define picked (random-unused-address used-numbers #:space-size space-size))
     (and (>= picked 0) (< picked space-size) (set-member? free-set picked))]))

(define prop:random-unused-address-sound
  (property random-unused-address-sound
            ([space-size (gen:integer-in 1 60)]
             [free-count (gen:integer-in 0 space-size)])
            (sound? space-size free-count)))

(module+ test
  (require rackunit)

  ;; hand-picked instances before trusting the generator/shrinker with them
  (test-case "returns a free address when some are free"
    (check-true (sound? 10 3)))

  (test-case "raises when nothing is free"
    (check-true (sound? 5 0)))

  (test-case "returns the only free address when exactly one is free"
    (check-true (sound? 200 1)))

  (test-case "returns a free address when everything is free"
    (check-true (sound? 30 30)))

  (define num-trials
    (or (let ([v (getenv "TR_PBT_TESTS")]) (and v (string->number v)))
        300))
  (check-property (make-config #:tests num-trials) prop:random-unused-address-sound))
