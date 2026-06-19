#lang racket/base
(provide setup-config!
         get-config
         get-output-path
         get-assets-path
         get-build-mode
         dev-mode?
         render-config-tag)
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

; A stable string of the config that the per-card renderer bakes into output.
; This feeds the build signature so two output targets with different such
; config get distinct signatures and never share a content-store entry.
;
; Today the only config that reaches per-card HTML is `fedi` (see
; generate-index.rkt). Deliberately EXCLUDED: output-path / mode / assets
; (they pick where files land, not their bytes) and domain / title (whole-site
; files only -- rss.xml, search.json -- which are regenerated every build, not
; cached). Folding those in would needlessly stop dev and release builds from
; sharing the store. Any new get-config read inside the render path must be
; added here.
(define (render-config-tag)
  (format "~s" (get-config 'fedi #f)))
