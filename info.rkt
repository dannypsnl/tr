#lang info
(define collection "tr")
(define deps
  '("base"
    "dirname"
    "gregor-lib"
    "mischief"
    "argo"
    "file-watchers"
    "scribble-html-lib"
    "scribble-text-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "gui-doc"))
(define scribblings '(("scribblings/tr.scrbl" (multi-page) ("site generator"))))
(define pkg-desc "A site generator")
(define version "0.1.0")
(define license '(Apache-2.0 OR MIT))
(define pkg-authors '(dannypsnl))

(define raco-commands
  '(("tr" (submod tr/command main) "run tr" #f)))
