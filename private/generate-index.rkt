#lang racket
(provide root?
         index-output-path
         produce-index!)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml
         "config.rkt"
         (only-in "../card.rkt"
                  self-addr toc/depth
                  tree
                  generate-toc
                  generate-context
                  generate-references
                  generate-backlinks
                  generate-related))

(define (root? addr)
  (string=? addr "index"))

(define (index-output-path addr)
  (if (root? addr)
      (build-path (get-output-path) "index.html")
      (build-path (get-output-path) addr "index.html")))

(define (produce-index! addr addr-maps-to-metajson)
  (define output-dir (build-path (get-output-path) addr))
  (make-directory* output-dir)
  (define out
    (open-output-file #:exists 'truncate/replace (index-output-path addr)))

  (define metaobj (hash-ref addr-maps-to-metajson addr))
  (define title (hash-ref metaobj 'title))
  (define lang (or (hash-ref metaobj 'lang #f) (get-config 'html-lang "")))
  (parameterize ([self-addr addr]
                 [toc/depth (if (hash-ref metaobj 'toc/depth) (hash-ref metaobj 'toc/depth) 2)]
                 [generate-root? (root? addr)])
    (output-xml
      (list
        (doctype 'html)
        (cond
          [(root? addr)
           (common-share #:title title #:lang lang
                         (div 'class: "top-wrapper"
                              (tree (build-path "_tmp" (string-append addr ".embed.html")))))]
          [else
           (common-share #:title title #:lang lang
                         (div 'class: "top-wrapper"
                              (main (tree (build-path "_tmp" (string-append addr ".embed.html"))))
                              (generate-toc))
                         (footer
                           (generate-context)
                           (generate-references)
                           (generate-backlinks)
                           (generate-related)))]))
      out))
  (close-output-port out))

(define generate-root? (make-parameter #f))

(define (common-share #:title this-title
                      #:lang [this-lang ""]
                      . content)
  (html
    'lang: (and (non-empty-string? this-lang) this-lang)
    (head
      (meta 'http-equiv: "Content-Type" 'content: "text/html; charset=utf-8")
      (meta 'name: "viewport" 'content: "width=device-width, initial-scale=1")
      (title this-title)
      (link 'rel: "stylesheet" 'href: "/katex.min.css")
      (link 'rel: "stylesheet" 'href: "/style.css")
      (get-config 'head '()))
    (body 'id: "whole"
          ; A site owns its page header: `header` in site.rkt replaces the
          ; default « Home link, and unlike the default it also renders on the
          ; root page, since a site's own header is usually wanted there too.
          (get-config 'header
                      (unless (generate-root?)
                        (a 'class: "link-home" 'href: "/" "« Home")))
          content)))
