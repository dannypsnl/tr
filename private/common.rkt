#lang racket
(provide
 file->json json->file
 non-local?
 m mm)
(require json
         net/http-easy)

(define (file->json path)
  (define in (open-input-file path))
  (define obj (read-json in))
  (close-input-port in)
  obj)
(define (json->file json path)
  (define out (open-output-file #:exists 'truncate/replace path))
  (write-json json out)
  (close-output-port out))

(define (latex->html tex #:display? [display? #f])
  (bytes->string/utf-8
   (response-body
    (post "http://localhost:8765"
          #:data tex
          #:headers (hash 'Display-Mode
                          (if display? "true" "false"))))))

(define (m formula) (latex->html formula))
(define (mm . forms)
  (define formula (apply string-append forms))
  (latex->html formula #:display? #t))

(define (non-local? addr)
  (not (string-contains? addr ":")))
