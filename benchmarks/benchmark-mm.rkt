#lang racket
(require racket/runtime-path)
(require net/http-easy)

;; Old version
(define (mm-old . forms)
  (define formula (apply string-append forms))
  (define js-code
    (format
     "import katex from \"npm:katex\"; let html = katex.renderToString(~s, { throwOnError: false, displayMode: true }); console.log(html)"
     formula))
  (with-output-to-string (λ () (system* (find-executable-path "deno") "eval" js-code))))

;; New version
(define (latex->html tex #:display? [display? #f])
  (bytes->string/utf-8
   (response-body
    (post "http://localhost:8765"
          #:data tex
          #:headers (hash 'Display-Mode
                          (if display? "true" "false"))))))
(define (mm-new . forms)
  (define formula (apply string-append forms))
  (latex->html formula #:display? #t))

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

  (define avg (/ (apply + times-list) times))
  (define min-time (apply min times-list))
  (define max-time (apply max times-list))

  (displayln (format "mean: ~a ms" avg))
  (displayln (format "min: ~a ms" min-time))
  (displayln (format "max: ~a ms" max-time)))

(define-runtime-path katex-service "../katex-service.ts")
(define (run-benchmarks)
  (match-define (list _stdout _stdin _pid _stderr stop-katex-service)
    (process* (find-executable-path "deno") "run" "--allow-net" katex-service))

  (displayln "=== KaTeX mm function benchmarking ===\n")
  (displayln "test formula：")
  (for ([formula test-formulas]
        [i (in-naturals 1)])
    (displayln (format "  ~a. ~a" i formula)))

  (displayln "\n\nWARNING：new version needs to run katex-service.ts")

  (define test-count 50)

  ;; Test old version
  (displayln "\n========================================")
  (benchmark "Old version (deno eval)"
             (lambda ()
               (for ([formula test-formulas])
                 (mm-old formula)))
             test-count)

  ;; Check service is running
  (displayln "\n========================================")
  (displayln "\nChecking katex-service...")
  (with-handlers ([exn:fail?
                   (lambda (_e)
                     (displayln "Error：cannot connect to katex-service！")
                     (exit 1))])
    (mm-new "x")
    (displayln "Connected to katex-service!"))

  ;; Test new version
  (benchmark "New version (service via HTTP)"
             (lambda ()
               (for ([formula test-formulas])
                 (mm-new formula)))
             test-count)

  (displayln "\n========================================")
  (displayln "Test Complete!")

  (stop-katex-service 'kill))

(module+ main
  (run-benchmarks))
