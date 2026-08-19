#lang racket/base
(provide card-metadata)
(require "private/common.rkt")

(define (card-metadata addr)
  (file->json (build-path "_tmp" (string-append addr ".metadata.json"))))
