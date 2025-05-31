#lang racket
(provide tr-title)

(define (tr-title text)
  (define current-scrbl-path (find-system-path 'run-file))
  (string-append "<h2>" text " "  "</h2>"))
