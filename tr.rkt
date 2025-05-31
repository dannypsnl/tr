#lang racket
(provide tr-title)
(require scribble/html/html)

(define (tr-title text)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-url (path-replace-extension current-scrbl-path ".html"))
  (define id (path->string (path-replace-extension current-scrbl-path "")))
  (define link-self (a 'href: (path->string self-url) 'target: "_parent" "[" id "]"))
  (h2 text " " link-self))
