#lang racket
(provide generate-index generate-root generate-toc
  common-share tree
  (rename-out [self-title title]
              [self-taxon taxon])
  transclude m mm tikzcd
  doctype div
  p ol ul li
  em strong
  code pre
  footer details summary
  h2)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require dirname
         data/queue)

(define-syntax-rule (define/provide-elements/not-empty tag ...)
  (begin (provide tag ...)
         (define (tag . args) (apply element/not-empty 'tag args)) ...))
(define/provide-elements/not-empty summary)

(define generate-index (make-parameter #f))
(define generate-root (make-parameter #f))
(define (self-addr)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-path (path->string (path-replace-extension current-scrbl-path "")))
  (string-trim (basename self-path) #px"\\.index|\\.embed"))
(define self-title (make-parameter #f))
(define self-taxon (make-parameter #f))

(define toc-queue (make-queue))
(define katex-queue (make-queue))

(define (tr-h2 addr text taxon)
  (define link-self (a 'class: "link-self" 'href: (string-append "/" addr) 'target: "_parent" "[" addr "]"))
  (if taxon
    (h2 (span 'class: "taxon" (string-append taxon ".")) "\n" text " " link-self)
    (h2 text " " link-self)))
(define (tr-title addr text taxon)
  (summary
    (header
      (tr-h2 addr text taxon)
      (div 'class: "metadata"))))

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
    (link 'rel: "stylesheet" 'href: "/style.css")
    (link 'rel: "stylesheet" 'href: "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css"
      'integrity: "sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP"
      'crossorigin: "anonymous")
    (script 'src: "/math.js")
    (script 'src: "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js"
      'integrity: "sha384-cMkvdD8LoxVzGF/RPUKAcvmm49FQ0oxwDF3BGKtDXcEc+T1b2N+teh/OJfpU0jr6"
      'crossorigin: "anonymous")
    )
   (body
      (cond [(generate-index) (a 'class: "link-home" 'href: "/" "<< Home")]
          [else (void)])
      content
      (script 'src: "/embedded.js")
      (if (queue-empty? katex-queue)
        (void)
        (script 'type: "text/javascript" (string-join (queue->list katex-queue) "\n"))))))

(define (tree . content)
  (article
    (details 'open: #t
      (tr-title (self-addr) (self-title) (self-taxon))
      content)))

(define (transclude addr)
  ; side effect
  (enqueue! toc-queue (a 'href: (string-append "#" addr) addr))
  
  ; add _tmp/<addr>.context.scrbl, but only when first time build embed.html
  (unless (generate-index)
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
