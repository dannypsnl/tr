#lang racket
(provide generate-index generate-root
  tree transclude m mm tikzcd
  doctype p code ol ul li)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require dirname
         data/queue)

(define generate-index (make-parameter #f))
(define generate-root (make-parameter #f))
(define toc-queue (make-queue))
(define katex-queue (make-queue))

(define (tr-title text taxon)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-path (path->string (path-replace-extension current-scrbl-path "")))
  (define id (string-trim (basename self-path) #px"\\.index|\\.embed"))
  (define link-self (a 'class: "link-self" 'href: (string-append "/" id) 'target: "_parent" "[" id "]"))
  (if taxon
    (h2 (span 'class: "taxon" (string-append taxon ".")) "\n" text " " link-self)
    (h2 text " " link-self)))

(define (generate-toc)
  (element 'nav 'id: "toc"
    (h2 "Table of Contents")
    (ul
      (for/list ([entry (queue->list toc-queue)]) (li entry)))))

(define (tree #:title title-text #:taxon [taxon #f] . content)
  (html
   (head
    (title title-text)
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
    (cond [(generate-root) ""]
          [(generate-index) (a 'class: "link-home" 'href: "/" "<< Home")]
          [else ""])
    (article
      (tr-title title-text taxon)
      content)
    (if (generate-index)
      (generate-toc)
      "")
    (script 'type: "text/javascript" (if (queue-empty? katex-queue) "" (string-join (queue->list katex-queue) "\n")))
    )))

(define (transclude address)
  ; side effect
  (define gen-id "#tree-1")
  (enqueue! toc-queue (span 'data-target: gen-id address))

  ; output
  (details 'open: "open"
    "<summary>" address "</summary>"
    (embed 'type: "text/html" 'src: (string-append "/" address "/embed.html"))))

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
  #| TODO
  2. produce _tmp/<job-id>/job.dvi
  2. produce _build/<job-id>.svg
  |#
  (make-directory* (build-path "_tmp" job-id))
  (define tex-path (build-path "_tmp" job-id "job.tex"))
  (define tex (open-output-file #:exists 'replace tex-path))
  (displayln "\\documentclass[crop,dvisvgm]{standalone}\n\\usepackage{quiver}\n\\begin{document}\n\\begin{tikzcd}" tex)
  (for-each (λ (s) (display s tex)) formula)
  (displayln "\n\\end{tikzcd}\n\\end{document}" tex)
  (close-output-port tex)

  (img 'class: "center"
    'src: (string-append "/" job-id ".svg")
    'alt: (string-append "figure " job-id))
  )
