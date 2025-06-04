#lang racket
(require file-watchers)

(define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) "content"))
(thread-wait
  (watch scrbl-list
    (λ (_paths)
      (system* (find-executable-path "make") "build"))
    (λ (_)
      (void))))
