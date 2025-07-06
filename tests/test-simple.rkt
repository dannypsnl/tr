#lang racket
(require "../metadata.rkt")

(module+ test
  (require rackunit)

  (test-case "compute-addr extracts filename"
    (check-equal? (compute-addr "test.scrbl") "test")
    (check-equal? (compute-addr "/path/to/file.scrbl") "file")))
