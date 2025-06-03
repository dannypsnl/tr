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
  (rename-out [set-self-title title]
              [set-self-taxon taxon]
              [self-date date]
              [self-author author])
  transclude m mm tikzcd
  mention
  doctype
  (except-out (all-from-out scribble/html/html) title)
  (all-from-out scribble/html/extra)
  summary
  article)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require data/queue)
(require "private/common.rkt")

(define-syntax-rule (define/provide-elements/not-empty tag ...)
  (begin (provide tag ...)
         (define (tag . args) (apply element/not-empty 'tag args)) ...))
(define/provide-elements/not-empty summary)

(define generate-index? (make-parameter #f))
(define generate-root? (make-parameter #f))

(define self-title (make-parameter #f))
(define (set-self-title . forms)
  (self-title forms))
(define self-taxon (make-parameter #f))
(define (set-self-taxon t)
  (self-taxon t))
(define self-date (make-parameter #f))
(define self-author (make-parameter #f))

(define toc-queue (make-queue))
(define katex-queue (make-queue))

(define (tr-h2 addr text taxon)
  (define link-to-self (a 'class: "link-self" 'href: (string-append "/" addr) 'target: "_parent" "[" addr "]"))
  (h2
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
      (summary (h2 title))
      (for/list ([addr addr-list])
        (tr-h2 addr (fetch-metadata addr 'title) (fetch-metadata addr 'taxon))))))
(define (generate-context) (footer-common "Context" 'context))
(define (generate-references) (footer-common "References" 'references))
(define (generate-backlinks) (footer-common "Backlinks" 'backlinks))
(define (generate-related) (footer-common "Related" 'related))

(define (generate-toc)
  (define entries (queue->list toc-queue))
  (cond
    [(empty? entries) (void)]
    [else
      (element 'nav 'id: "toc"
        (h2 "Table of Contents")
          (ul (for/list ([e entries]) e)))]))

(define (common-share . content)
  (html
   (head
    (title (self-title))
    (link 'rel: "stylesheet" 'href: "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css"
      'integrity: "sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP"
      'crossorigin: "anonymous")
    (link 'rel: "stylesheet" 'href: "/style.css")

    (script 'src: "https://cdn.jsdelivr.net/npm/minisearch@7.1.2/dist/umd/index.min.js")
    (script 'src: "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js"
      'integrity: "sha384-cMkvdD8LoxVzGF/RPUKAcvmm49FQ0oxwDF3BGKtDXcEc+T1b2N+teh/OJfpU0jr6"
      'crossorigin: "anonymous")
    (script 'src: "/tiny.js")
    (script 'src: "/math.js"))
   (body 'id: "whole"
      (dialog 'id: "search-dialog"
        (input 'type: "text" 'id: "search-bar"
          'spellcheck: "false" 'autocomplete: "off"
          'placeholder: "Start typing a note title or ID")
        (div 'id: "search-result"))
      (cond
        [(generate-root?) (void)]
        [(generate-index?) (a 'class: "link-home" 'href: "/" "<< Home")]
        [else (void)])
      content
      (when (or (generate-index?) (generate-root?)) (script 'src: "/fullTextSearch.js"))
      (unless (queue-empty? katex-queue)
        (script 'type: "text/javascript" (string-join (queue->list katex-queue) "\n"))))))

(define (tree . content)
  (details 'open: #t
    (summary
      (header
        (tr-h2 (self-addr) (self-title) (self-taxon))
        (div 'class: "metadata"
          (ul
            (when (self-date) (li (self-date)))
            (when (self-author) (li (self-author)))))))
    content))

(define (transclude addr)
  ; side effect
  (enqueue! toc-queue
    (li (a 'href: (string-append "#" addr) (fetch-metadata addr 'title))))

  ; output
  (details 'open: #t
    (summary
      (header
        (tr-h2 addr (fetch-metadata addr 'title) (fetch-metadata addr 'taxon))
        (div 'class: "metadata"
          (ul
            (li (fetch-metadata addr 'date))
            (li (fetch-metadata addr 'author))))))
    (iframe 'class: "embedded" 'id: addr
      'scrolling: "no"
      'src: (string-append "/" addr "/embed.html"))))

(define (mention addr [title #f])
  (define url (string-append "/" addr))
  (a 'class: "mention"
     'target: "_parent"
     'href: url
     (if title title (fetch-metadata addr 'title))))

(define (m formula)
  (define katex-id ((compose symbol->string gensym) 'm))
  (define js-code (format "katex.render(~s, document.getElementById(~s), { throwOnError: false, macros: document.macros });" formula katex-id))
  (enqueue! katex-queue js-code)
  (span 'id: katex-id "formula"))

(define (mm formula)
  (define katex-id ((compose symbol->string gensym) 'mm))
  (define js-code (format "katex.render(~s, document.getElementById(~s), { throwOnError: false, macros: document.macros, displayMode: true });" formula katex-id))
  (enqueue! katex-queue js-code)
  (span 'id: katex-id formula))

(define (tikzcd . formula)
  (define job-id (symbol->string (gensym 'tex)))
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
