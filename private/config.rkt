#lang racket/base
(provide setup-config!
         get-config
         get-output-path
         get-assets-path
         get-build-mode
         dev-mode?)
(require "common.rkt")

(define configuration #f)
(define (setup-config! filepath)
  (set! configuration (file->json filepath)))

(define (get-config key default)
  (hash-ref configuration key default))

(define (get-output-path)
  (get-config 'output-path "_build"))

(define (get-assets-path)
  (get-config 'assets '("assets")))

(define (get-build-mode)
  (get-config 'mode "release"))

(define (dev-mode?)
  (equal? "dev" (get-build-mode)))
