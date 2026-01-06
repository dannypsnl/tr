#lang racket
(provide
 file->json json->file
 non-local?
 m mm)
(require json)

(define (file->json path)
  (define in (open-input-file path))
  (define obj (read-json in))
  (close-input-port in)
  obj)
(define (json->file json path)
  (define out (open-output-file #:exists 'truncate/replace path))
  (write-json json out)
  (close-output-port out))

(define (m formula)
  (define js-code
    (format
     "import katex from \"npm:katex\"; let html = katex.renderToString(~s, { throwOnError: false }); console.log(html)"
     formula))
  (with-output-to-string (λ () (system* (find-executable-path "deno") "eval" js-code))))

(define (mm . forms)
  (define formula (apply string-append forms))
  (define js-code
    (format
     "import katex from \"npm:katex\"; let html = katex.renderToString(~s, { throwOnError: false, displayMode: true }); console.log(html)"
     formula))
  (with-output-to-string (λ () (system* (find-executable-path "deno") "eval" js-code))))

(define (non-local? addr)
  (not (string-contains? addr ":")))
