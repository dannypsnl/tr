#lang racket
(provide
  generate-toc
  generate-context
  generate-references
  generate-backlinks
  generate-related

  common-share tree

  generate-index?
  generate-root?
  (rename-out [pre* pre]
              [set-title title]
              [ignore taxon]
              [ignore date]
              [ignore author]
              [ignore author/literal]
              [ignore tm])
  transclude
  m mm tikzcd texfig
  mention external
  hentry
  doctype
  (except-out (all-from-out scribble/html/html) title pre)
  (all-from-out scribble/html/extra)
  summary
  article
  footer svg path)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require data/queue)
(require (only-in scribble/text disable-prefix))
(require "private/common.rkt")

(define/provide-elements/not-empty summary path)

(define generate-index? (make-parameter #f))
(define generate-root? (make-parameter #f))

(define self-title (make-parameter #f))
(define (set-title . forms)
  (self-title forms))
(define (ignore . _) (void))

(define (tr-h1 addr text taxon)
  (define url
    (if (string=? "index" addr)
      "/"
      (string-append "/" addr)))
  (define link-to-self (a 'class: "link-self" 'href: url 'target: "_parent" "[" addr "]"))
  (h1
    (when taxon
      (list (span 'class: "taxon" (string-append taxon ".")) " "))
    text
    " "
    link-to-self))

(define (fetch-metadata addr key [default #f])
  (hash-ref (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))) key default))
(define (footer-common title key)
  (define addr-list (fetch-metadata (self-addr) key '()))
  (unless (empty? addr-list)
    (details 'open: #t 'id: (symbol->string key)
      (summary (h1 title))
      (for/list ([addr addr-list])
        (tr-h1 addr (fetch-metadata addr 'title) (fetch-metadata addr 'taxon))))))
(define (generate-context) (footer-common "Context" 'context))
(define (generate-references) (footer-common "References" 'references))
(define (generate-backlinks) (footer-common "Backlinks" 'backlinks))
(define (generate-related) (footer-common "Related" 'related))

(define (generate-toc)
  (define entries (fetch-metadata (self-addr) 'transclude))
  (define (wrap addr)
    (define entries (fetch-metadata addr 'transclude))
    (li (a 'class: "toc" 'href: (string-append "#" addr)
      (fetch-metadata addr 'title)
      (if (empty? entries) (void) (ol (for/list ([addr entries]) (wrap addr)))))))
  (define lst
    (for/list ([addr entries])
      (wrap addr)))
  (cond
    [(empty? entries) (void)]
    [else
      (element 'nav 'id: "toc"
        (h1 "Table of Contents")
          (ol lst))]))

(define (common-share . content)
  (html
   (head
    (meta 'http-equiv: "Content-Type" 'content: "text/html; charset=utf-8")
    (title (self-title))
    (link 'rel: "stylesheet" 'href: "/katex.min.css")
    (link 'rel: "stylesheet" 'href: "/style.css")

    (script 'src: "/minisearch/index.min.js")
    (script 'src: "/tiny.js"))
   (body 'id: (if (generate-index?) "whole" "embed-whole")
      (dialog 'id: "search-dialog"
        (input 'type: "text" 'id: "search-bar"
          'spellcheck: "false" 'autocomplete: "off"
          'placeholder: "Start typing a note title or ID")
        (div 'id: "search-result"))
      (cond
        [(generate-root?) (void)]
        [(generate-index?) (a 'class: "link-home" 'href: "/" "&#171; Home")]
        [else (void)])
      content
      (when (or (generate-index?) (generate-root?)) (script 'src: "/fullTextSearch.js")))))

(define (tree . content)
  (define meta-queue (make-queue))
  (when (fetch-metadata (self-addr) 'date)
    (enqueue! meta-queue (li (fetch-metadata (self-addr) 'date))))
  (define authors
    (for/list ([addr (fetch-metadata (self-addr) 'authors)])
      (a 'class: "link-self" 'href: (string-append "/" addr) (fetch-metadata addr 'title))))
  (define name-authors (fetch-metadata (self-addr) 'name-authors))
  (unless (empty? (append authors name-authors))
    (enqueue! meta-queue (li (add-between (append authors name-authors) ", "))))

  (details 'open: #t
    (summary
      (header
        (tr-h1 (self-addr) (self-title) (fetch-metadata (self-addr) 'taxon))
        (div 'class: "metadata"
          (ul
            (add-between (queue->list meta-queue) " &#183; ")))))
    content))

(define (transclude #:open [open? #t] addr)
  (details 'open: open? 'id: addr
    (summary
      (header
        (tr-h1 addr (fetch-metadata addr 'title) (fetch-metadata addr 'taxon))
        (div 'class: "metadata"
          (ul
            (li (fetch-metadata addr 'date))
            (li (fetch-metadata addr 'author))))))
    (file->string (string-append "_tmp/" addr ".embed.html"))))

(define (pre* . content)
  (disable-prefix (pre content)))

(define (external url)
  (a 'href: url 'target: "_blank" url))

(define (mention #:title [title #f] addr)
  (define url (string-append "/" addr))
  (a 'class: "mention"
     'target: "_parent"
     'href: url
     (if title title (fetch-metadata addr 'title))))

(define (texfig . formula)
  (define job-id (symbol->string (gensym (self-addr))))
  (make-directory* (build-path "_tmp" job-id))
  (define tex-path (build-path "_tmp" job-id "job.tex"))
  (define tex (open-output-file #:exists 'replace tex-path))
  (displayln "\\documentclass[crop,dvisvgm]{standalone}\n" tex)
  (displayln "\\usepackage{quiver}\n" tex)
  (displayln "\\usepackage{tikz}\n" tex)
  (displayln "\\usetikzlibrary{spath3, intersections, backgrounds}\n" tex)
  (displayln "\\begin{document}\n" tex)
  (for-each (λ (s) (display s tex)) formula)
  (displayln "\n\\end{document}" tex)
  (close-output-port tex)

  (img 'class: "center"
    'src: (string-append "/" job-id ".svg")
    'alt: (string-append "figure " job-id)))

(define (tikzcd . formula)
  (define job-id (symbol->string (gensym (self-addr))))
  (make-directory* (build-path "_tmp" job-id))
  (define tex-path (build-path "_tmp" job-id "job.tex"))
  (define tex (open-output-file #:exists 'replace tex-path))
  (displayln "\\documentclass[crop,dvisvgm]{standalone}\n\\usepackage{quiver}\n\\begin{document}\n\\begin{tikzcd}" tex)
  (for-each (λ (s) (display s tex)) formula)
  (displayln "\n\\end{tikzcd}\n\\end{document}" tex)
  (close-output-port tex)

  (img 'class: "center"
    'src: (string-append "/" job-id ".svg")
    'alt: (string-append "figure " job-id)))

(define (hentry description)
  (div 'class: "h-entry" 'hidden: #t (p 'class: "e-content" description)))
