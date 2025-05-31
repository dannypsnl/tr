#lang racket
(provide tree transclude p)
(require scribble/html/html
         scribble/html/extra
         scribble/html/xml)

(define (tr-title text)
  (define current-scrbl-path (find-system-path 'run-file))
  (define self-url (path-replace-extension current-scrbl-path ".html"))
  (define id (path->string (path-replace-extension current-scrbl-path "")))
  (define link-self (a 'href: (path->string self-url) 'target: "_parent" "[" id "]"))
  (h2 text " " link-self))

(define toc-list (mutable-set))
(define (generate-toc)
  ; (for/set ([entry toc-list])
  ;   )
  (element 'nav 'id: "toc"
    (h2 "Table of Contents")
    )
  )

(define (tree #:title title-text #:taxon [taxon #f] . content)
  (html
   (head
    (title title-text)
    (link 'rel: "stylesheet" 'href: "style.css"))
   (body
    (article
      (tr-title title-text)
      content)
    (generate-toc))))

(define (transclude address)
  (details 'open: "open"
    "<summary>" address "</summary>"
    (embed 'type: "text/html" 'src: (string-append address ".html"))))
