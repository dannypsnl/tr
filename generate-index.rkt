#lang racket
(provide root?
         produce-indexes)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml
         "private/config.rkt"
         (only-in "card.rkt"
                  self-addr toc/depth
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
    (define output-dir (build-path (get-output-path) addr))
    (make-directory* output-dir)
    (define out
      (open-output-file #:exists 'truncate/replace
                        (if (root? addr)
                            (build-path (get-output-path) "index.html")
                            (build-path (get-output-path) addr "index.html"))))

    (define metaobj (hash-ref addr-maps-to-metajson addr))
    (define title (hash-ref metaobj 'title))
    (parameterize ([self-addr addr]
                   [toc/depth (if (hash-ref metaobj 'toc/depth) (hash-ref metaobj 'toc/depth) 2)]
                   [generate-root? (root? addr)])
      (output-xml
       (list
        (doctype 'html)
        (cond
          [(root? addr)
           (define fedi (get-config 'fedi #f))
           (common-share #:title title
                         #:fedi-validation (and fedi
                                                (link 'rel: "me"
                                                      'href: (format "https://~a/@~a" (hash-ref fedi 'site) (hash-ref fedi 'handle))))
                         (div 'class: "top-wrapper"
                              (tree (build-path "_tmp" (string-append addr ".embed.html")))))]
          [else
           (define fedi (get-config 'fedi #f))
           (common-share #:title title
                         #:fedi-signature
                         (and fedi
                              (meta 'name: "fediverse:creator"
                                    'content: (format "@~a@~a" (hash-ref fedi 'handle) (hash-ref fedi 'site))))
                         (div 'class: "top-wrapper"
                              (main (tree (build-path "_tmp" (string-append addr ".embed.html"))))
                              (generate-toc))
                         (footer
                          (generate-context)
                          (generate-references)
                          (generate-backlinks)
                          (generate-related)))]))
       out))
    (close-output-port out)))

(define generate-root? (make-parameter #f))

(define (common-share #:title this-title
                      #:fedi-validation [fedi-validation #f]
                      #:fedi-signature [fedi-signature #f]
                      . content)
  (html
   (head
    (meta 'http-equiv: "Content-Type" 'content: "text/html; charset=utf-8")
    (meta 'name: "viewport" 'content: "width=device-width, initial-scale=1")
    fedi-signature
    (title this-title)
    (link 'rel: "stylesheet" 'href: "/katex.min.css")
    (link 'rel: "stylesheet" 'href: "/style.css")
    fedi-validation

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
