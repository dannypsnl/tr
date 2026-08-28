#lang racket
(provide base36->int
         int->base36
         random-unused-address)
(require racket/random)

(define alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define base (string-length alphabet))
(define address-digits 4)
(define space-size (expt base address-digits))

(define (base36->int text)
  (define len (string-length text))
  (let loop ([sum 0]
             [place 1]
             [r (sub1 len)])
    (cond
      [(< r 0) sum]
      [else
       (define x (string-ref text r))
       (define digit-value (string-find alphabet (build-string 1 (λ (_) x))))
       (and digit-value (loop (+ sum (* place digit-value)) (* place base) (sub1 r)))])))

(define (int->base36 number)
  (define (convert n acc)
    (match n
      [0 acc]
      [_ (define q (quotient n base))
       (define r (remainder n base))
       (convert q (string-append (string (string-ref alphabet r)) acc))]))

  (define (pad-zeros str len)
    (define str-len (string-length str))
    (if (< str-len len)
        (string-append (make-string (- len str-len) #\0) str)
        str))

  (pad-zeros (convert number "") address-digits))

; Let p = free/space-size be the per-try success probability. We want the
; smallest k with (1-p)^k <= ε (the "cap tries in a row all fail" bound),
; whose exact solution is k = ln(1/ε) / ln(1/(1-p)).
;
; We use cap = m/p instead (m = ln(1/ε), 1/p = E[tries]), because
; ln(1-p) <= -p holds for every p in (0,1), i.e. ln(1/(1-p)) >= p, so
;   m/ln(1/(1-p)) <= m/p
; meaning m/p is always >= the exact k — a safe over-approximation but
; much cheaper to compute (compare to ln(1-p), which is also numerically dicey as p -> 1)
; at the cost of a few extra retries in the worst case. ε is the
; actual knob: the probability of an unlucky fallback we're willing to
; accept; m is just its inverse-log.
(define fallback-epsilon 1e-9)
(define fallback-safety-margin (- (log fallback-epsilon)))

(define (exhaustive-random-unused numbers-set)
  (random-ref (sequence-filter (lambda (x) (not (set-member? numbers-set x)))
                               (in-inclusive-range 0 (sub1 space-size)))))

; random-unused-address means to compute an address that in 36^4 addr in the space that is unused
;
; parameters
; + numbers-set: a set of already-used base36 values (as integers), scoped
;   to one prefix
;
; Returns a uniformly random unused integer in [0, space-size)
(define (random-unused-address numbers-set)
  (define used (set-count numbers-set))
  (define free (- space-size used))
  (when (<= free 0)
    (error 'random-unused-address "All address in this space is used"))

  (define expected-tries (/ space-size free)) ; = 1/p
  (define cap (min space-size (inexact->exact (ceiling (* fallback-safety-margin expected-tries)))))
  (or (for/or ([_ (in-range cap)])
        (define candidate (random space-size))
        (and (not (set-member? numbers-set candidate)) candidate))
      (exhaustive-random-unused numbers-set)))

(module+ test
  (require rackunit)

  (check-false (base36->int ",,,"))

  (check-equal? (int->base36 10) "000A")
  (check-equal? (int->base36 35) "000Z")
  (check-equal? (int->base36 36) "0010")
  (check-equal? (int->base36 1000) "00RS")
  (check-equal? (int->base36 0) "0000")
  (check-equal? (int->base36 123456) "2N9C")
  (check-equal? (int->base36 (base36->int "ZZZZ")) "ZZZZ")

  (let ([picked (random-unused-address (set 0 1 2))])
    (check-true (and (>= picked 0) (< picked space-size)))
    (check-false (set-member? (set 0 1 2) picked)))

  (let* ([all-but-one (set-remove (list->set (range space-size)) 42)]
         [picked (random-unused-address all-but-one)])
    (check-equal? picked 42))

  (check-exn exn:fail?
             (lambda ()
               (random-unused-address (list->set (range space-size))))))
