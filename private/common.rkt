#lang racket
(provide self-addr
  file->json)
(require dirname json)

(define (self-addr)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-path (path->string (path-replace-extension current-scrbl-path "")))
  (string-trim (basename self-path) #px"\\.index|\\.embed|\\.meta"))

(define (file->json path)
  (define in (open-input-file path))
  (define obj (read-json in))
  (close-input-port in)
  obj)
