#lang racket
(provide root?
         produce-indexes)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml
         (only-in "card.rkt"
            self-addr
            tree
            generate-toc
            generate-context
            generate-references
            generate-backlinks
            generate-related))

(define (root? addr)
  (string=? addr "index"))

(define (produce-indexes addr-list excludes addr-maps-to-metajson)
  (for ([addr addr-list]
        #:unless (set-member? excludes addr))
    (printf "generate ~a/index.html ~n" addr)
    (define output-dir (build-path "_build/" addr))
    (make-directory* output-dir)
    (define out
      (open-output-file #:exists 'truncate/replace
        (if (root? addr)
          (build-path "_build" "index.html")
          (build-path "_build" addr "index.html"))))
    (define title (hash-ref (hash-ref addr-maps-to-metajson addr) 'title))
    (parameterize ([self-addr addr]
                   [generate-root? (root? addr)])
    (output-xml
      (list
        (doctype 'html)
        (if (root? addr)
          (common-share #:title title
            (div 'class: "top-wrapper"
              (tree (build-path "_tmp" (string-append addr ".embed.html")))))
          (common-share #:title title
            (div 'class: "top-wrapper"
              (main (tree (build-path "_tmp" (string-append addr ".embed.html"))))
            (generate-toc))
            (footer
              (generate-context)
              (generate-references)
              (generate-backlinks)
              (generate-related))))
        )
      out))
    (close-output-port out)))

(define generate-root? (make-parameter #f))

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
      (unless (generate-root?)
        (a 'class: "link-home" 'href: "/" "« Home"))
      content
      (script 'src: "/fullTextSearch.js"))))
