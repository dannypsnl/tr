#lang racket
(provide generate-index
  tree transclude p)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)
(require dirname
         data/queue)

(define generate-index (make-parameter #f))

(define (tr-title text taxon)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-path (path->string (path-replace-extension current-scrbl-path "")))
  (define id (string-trim (basename self-path) #px"\\.index|\\.embed"))
  (define link-self (a 'href: (string-append "/" id) 'target: "_parent" "[" id "]"))
  (if taxon
    (h2 (span 'class: "taxon" (string-append taxon ".")) "\n" text " " link-self)
    (h2 text " " link-self)))

(define toc-queue (make-queue))
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
    )
   (body
    (article
      (tr-title title-text taxon)
      content)
    (if (generate-index)
      (generate-toc)
      "")
    )))

(define (transclude address)
  ; side effect
  (define gen-id "#tree-1")
  (enqueue! toc-queue (span 'data-target: gen-id address))

  ; output
  (details 'open: "open"
    "<summary>" address "</summary>"
    (embed 'type: "text/html" 'src: (string-append "/" address "/embed.html"))))
