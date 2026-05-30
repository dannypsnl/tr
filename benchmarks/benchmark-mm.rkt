#lang racket
(require racket/runtime-path)

;; Old version
(define (mm-old . forms)
  (define formula (apply string-append forms))
  (define js-code
    (format
      "import katex from \"katex\"; let html = katex.renderToString(~s, { throwOnError: false, displayMode: true }); console.log(html)"
      formula))
  (with-output-to-string (λ () (system* (find-executable-path "bun") "-e" js-code))))

(define test-formulas
  '("x^2 + y^2 = z^2"
    "\\int_{0}^{\\infty} e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}"
    "\\sum_{n=1}^{\\infty} \\frac{1}{n^2} = \\frac{\\pi^2}{6}"
    "\\nabla \\times \\vec{E} = -\\frac{\\partial \\vec{B}}{\\partial t}"
    "E = mc^2"))


(define (benchmark name fn times)
  (displayln (format "\nTest ~a:" name))
  (displayln (format "Times: ~a" times))

  (define times-list '())

  (for ([i (in-range times)])
    (define-values (_result _cpu real _gc)
      (time-apply fn '()))
    (set! times-list (cons real times-list))
    (when (= (modulo i 10) 0)
      (displayln (format "  complete ~a/~a..." i times))))

  (define avg (exact->inexact (/ (apply + times-list) times)))
  (define min-time (apply min times-list))
  (define max-time (apply max times-list))

  (displayln (format "mean: ~a ms" avg))
  (displayln (format "min: ~a ms" min-time))
  (displayln (format "max: ~a ms" max-time)))

(define-runtime-path katex-stdio-bun "../katex-stdio-bun.ts")
(define-runtime-path katex-stdio-deno "../katex-stdio-deno.ts")
(define-runtime-path katex-stdio-node "../katex-stdio-node.mjs")

(define (read-until-null in)
  (define out (open-output-bytes))
  (let loop ()
    (define b (read-byte in))
    (cond
      [(eof-object? b) (error 'read-until-null "eof before sentinel")]
      [(= b 0) (bytes->string/utf-8 (get-output-bytes out))]
      [else (write-byte b out) (loop)])))

(define (make-stdio-fn stdin-port stdout-port [prefix ""])
  (lambda forms
    (define formula (apply string-append forms))
    (fprintf stdin-port "~a~a\n" prefix formula)
    (flush-output stdin-port)
    (read-until-null stdout-port)))
(define (run-benchmarks)
  (displayln "=== KaTeX mm function benchmarking ===\n")
  (displayln "test formula：")
  (for ([formula test-formulas]
        [i (in-naturals 1)])
    (displayln (format "  ~a. ~a" i formula)))

  (define test-count 50)

  ;; Test old version
  (displayln "\n========================================")
  (benchmark "Old version (bun -e)"
             (lambda ()
               (for ([formula test-formulas])
                 (mm-old formula)))
             test-count)

  (define (run-stdio-benchmark label exe #:prefix [prefix ""] . args)
    (displayln "\n========================================")
    (displayln (format "Starting ~a stdio worker..." label))
    (match-define (list s-out s-in _pid _err stop-worker)
      (apply process* (find-executable-path exe) args))
    (define mm-stdio (make-stdio-fn s-in s-out prefix))

    (displayln (format "\nChecking ~a stdio worker..." label))
    (let loop ([retries 20])
      (with-handlers ([exn:fail?
                       (lambda (_e)
                         (cond
                           [(zero? retries)
                            (displayln (format "Error：~a stdio worker not responding！" label))
                            (stop-worker 'kill)
                            (exit 1)]
                           [else
                            (sleep 0.25)
                            (loop (sub1 retries))]))])
        (mm-stdio "x")
        (displayln (format "~a stdio worker ready!" label))))

    (benchmark (format "New version (~a stdio)" label)
               (lambda ()
                 (for ([formula test-formulas])
                   (mm-stdio formula)))
               test-count)

    (stop-worker 'kill)
    (sleep 0.2))

  (run-stdio-benchmark "deno" "deno" #:prefix "d" "run" katex-stdio-deno)
  (run-stdio-benchmark "bun" "bun" #:prefix "d" "run" katex-stdio-bun)

  (displayln "\n========================================")
  (displayln "Test Complete!"))

(module+ main
  (run-benchmarks))
