#lang racket
(provide
  file->json json->file
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
    (format "import katex from \"npm:katex\"; let html = katex.renderToString(~s, { throwOnError: false }); console.log(html)" formula))
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "deno") "eval" js-code))
  (get-output-string out))

(define (mm . forms)
  (define formula (apply string-append forms))
  (define js-code
    (format "import katex from \"npm:katex\"; let html = katex.renderToString(~s, { throwOnError: false, displayMode: true }); console.log(html)" formula))
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (system* (find-executable-path "deno") "eval" js-code))
  (get-output-string out))
