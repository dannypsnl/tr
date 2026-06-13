#lang racket
(require "../metadata.rkt"
         "../card.rkt")

(module+ test
  (require rackunit)

  (test-case "compute-addr extracts filename"
    (check-equal? (compute-addr "test.scrbl") "test")
    (check-equal? (compute-addr "/path/to/file.scrbl") "file"))

  (test-case "mention/hidden renders nothing"
    ; the visible link lives in @include'd external html; the scrbl form only
    ; exists to feed the backlink graph, so it must produce no output.
    (check-pred void? (mention/hidden "agda-target"))
    (check-pred void? (mention/hidden "agda-target" "ignored body"))))
