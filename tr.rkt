#lang racket
(provide
  generate-toc
  generate-related
  generate-metadata
  common-share tree

  generate-index?
  generate-root?
  (rename-out [set-self-title title]
              [set-self-taxon taxon])
  transclude m mm tikzcd
  mention
  doctype div
  (rename-out [collect-p p])
  ol ul li
  a
  em strong
  code pre
  footer details summary
  h2)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require dirname
         json
         data/queue)

(define-syntax-rule (define/provide-elements/not-empty tag ...)
  (begin (provide tag ...)
         (define (tag . args) (apply element/not-empty 'tag args)) ...))
(define/provide-elements/not-empty summary)

(define generate-index? (make-parameter #f))
(define generate-root? (make-parameter #f))

(define content-queue (make-queue))
(define (collect-p . content)
  (for ([t content]
        #:when (string? t))
    (enqueue! content-queue t))

  (p content))
; This is a side effect procedure creates <addr>.metadata.json, will produce no HTML
; This file can be lookup and build fullText search
(define (generate-metadata)
  (define addr (self-addr))
  (define taxon (self-taxon))
  (define title (self-title))

  (define collected-text (string-join (queue->list content-queue) " "))

  (define metadata
    (make-hasheq (list
      (cons 'id addr)
      (cons 'title (xml->string title))
      (cons 'taxon taxon)
      (cons 'text collected-text))))

  (define out (open-output-file #:exists 'replace (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (write-json	metadata out)
  (close-output-port out)

  (void))

(define (self-addr)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-path (path->string (path-replace-extension current-scrbl-path "")))
  (string-trim (basename self-path) #px"\\.index|\\.embed"))
(define self-title (make-parameter #f))
(define (set-self-title . forms)
  (self-title forms))
(define self-taxon (make-parameter #f))
(define (set-self-taxon t)
  (self-title t))

(define toc-queue (make-queue))
(define katex-queue (make-queue))

(define (tr-h2 addr text taxon)
  (define link-self (a 'class: "link-self" 'href: (string-append "/" addr) 'target: "_parent" "[" addr "]"))
  (if taxon
    (h2 (span 'class: "taxon" (string-append taxon ".")) "\n" text " " link-self)
    (h2 text " " link-self)))

(define (mention addr [title #f])
  ; side effect
  #| TODO:
    1. update backlinks
    2. update here related
    3. update here references

    Notice that 2 and 3 are distinguish by taxon, and hence, we will need to produce metadata when embed build
  |#

  ; output
  (define url (string-append "/" addr))
  (a 'href: url title))

(define (generate-related)
  ; TODO: list related cards
  (void))

(define (generate-toc)
  (define entries (queue->list toc-queue))
  (cond
    [(empty? entries) (void)]
    [else
      (element 'nav 'id: "toc"
        (h2 "Table of Contents")
          (ul (for/list ([e entries]) (li e))))]))

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
    (when (or (generate-index?) (generate-root?)) (script 'src: "/fullTextSearch.js"))
    (script 'src: "/math.js"))
   (body
      (cond
        [(generate-root?) (void)]
        [(generate-index?) (a 'class: "link-home" 'href: "/" "<< Home")]
        [else (void)])
      content
      (script 'src: "/embedded.js")
      (unless (queue-empty? katex-queue)
        (script 'type: "text/javascript" (string-join (queue->list katex-queue) "\n"))))))

(define (tree . content)
  (article
    (details 'open: #t
      (summary
        (header
          (tr-h2 (self-addr) (self-title) (self-taxon))
          (div 'class: "metadata")))
      content)))

(define (transclude addr)
  ; side effect
  (enqueue! toc-queue (a 'href: (string-append "#" addr) addr))
  
  ; add _tmp/<addr>.context.scrbl, but only when first time build embed.html
  (unless (generate-index?)
    (define addr-ctx (open-output-file #:exists 'append (build-path "_tmp" (string-append addr "." "context" ".scrbl"))))
    (displayln (xml->string (tr-h2 (self-addr) (self-title) (self-taxon))) addr-ctx)
    (close-output-port addr-ctx))

  ; output
  (iframe 'class: "embedded" 'id: addr
    'scrolling: "no"
    'src: (string-append "/" addr "/embed.html")))

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
