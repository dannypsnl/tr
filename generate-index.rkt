#lang racket
(provide generate-mode
         common-share)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)

(define generate-mode (make-parameter #f))
#| we use strict match so other values will crash the program, are invalid input |#
(define (generate-root?)
  (case (generate-mode)
    [(index) #f]
    [(root) #t]))

(define (common-share #:title this-title . content)
  (html
   (head
    (meta 'http-equiv: "Content-Type" 'content: "text/html; charset=utf-8")
    (meta 'name: "viewport" 'content: "width=device-width, initial-scale=1")
    (title this-title)
    (link 'rel: "stylesheet" 'href: "/katex.min.css")
    (link 'rel: "stylesheet" 'href: "/style.css")

    (script 'src: "/minisearch/index.min.js")
    (script 'src: "/tiny.js"))
   (body 'id: "whole"
      (dialog 'id: "search-dialog"
        (input 'type: "text" 'id: "search-bar"
          'spellcheck: "false" 'autocomplete: "off"
          'placeholder: "Start typing a note title or ID")
        (div 'id: "search-result"))
      (cond
        [(generate-root?) (void)]
        [else (a 'class: "link-home variant-strip-embed" 'href: "/" "&#171; Home")])
      content
      (script 'class: "variant-strip-embed" 'src: "/fullTextSearch.js"))))
