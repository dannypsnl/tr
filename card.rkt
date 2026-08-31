#lang racket
(provide
  self-addr

  generate-toc
  generate-context
  generate-references
  generate-backlinks
  generate-related

  tree

  toc/depth
  (rename-out [pre* pre]
              [pre* bibtex]
              [ignore title]
              [ignore taxon]
              [ignore html/lang]
              [ignore date]
              [ignore author]
              [ignore author/literal]
              [ignore doi]
              [ignore orcid]
              [ignore meta/text]
              [ignore meta/link]
              [ignore custom/metadata]
              [ignore tm]
              [ignore tr/code]
              [ignore mention/hidden]
              [ignore tr/depends])
  transclude
  tr/card
  card-counting
  reset-metadata-cache!
  m mm tikzcd texfig typst
  mention note
  external
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
(require "private/common.rkt"
         "private/metadata-store.rkt")

(define/provide-elements/not-empty summary path)

(define self-addr (make-parameter #f))

(define toc/depth (make-parameter 2))
(define (ignore . _) (void))

(define (addr->url addr)
  (cond
    [(string=? "index" addr) "/"]
    [else (string-append "/" addr)]))

; numbered? controls whether the taxon gets a CSS-counter-generated number:
; a page's own heading (rendered by `tree`) has no parent context to count
; against, so it stays bare; a heading rendered where a page is transcluded
; (by `transclude`) sits inside the enclosing .tr-body/#toc counter scope, so
; it gets numbered the same way TOC entries and tr/card entries do.
(define (tr-h1 addr text taxon #:numbered? [numbered? #f])
  (define url (addr->url addr))
  (define link-to-self (a 'class: "link-self" 'href: url 'target: "_parent" "[" addr "]"))
  (define taxon-span
    (cond
      [(and numbered? taxon)
       (list (span 'class: "taxon numbered" (string-append taxon " ")) " ")]
      [numbered?
       (list (span 'class: "taxon numbered" "") " ")]
      [taxon
       (list (span 'class: "taxon" (string-append taxon ".")) " ")]
      [else (void)]))
  (h1 taxon-span
      text
      " "
      link-to-self))

(define cached-metadata (make-hash))
(define (reset-metadata-cache!) (hash-clear! cached-metadata))
(define (fetch-metadata addr key [default #f])
  (if (hash-ref cached-metadata addr #f)
      (hash-ref (hash-ref cached-metadata addr) key default)
      (let ([json (metadata-store-ref addr)])
        (hash-set! cached-metadata addr json)
        (hash-ref json key default))))
(define (footer-common title key)
  (define addr-list (fetch-metadata (self-addr) key '()))
  (unless (empty? addr-list)
    (details 'open: #t 'id: (symbol->string key)
             (summary (h1 title))
             (for/list ([addr addr-list])
               (tr-h1 addr (literal (fetch-metadata addr 'title)) (fetch-metadata addr 'taxon))))))
(define (generate-context) (footer-common "Context" 'context))
(define (generate-references) (footer-common "References" 'references))
(define (generate-backlinks) (footer-common "Backlinks" 'backlinks))
(define (generate-related) (footer-common "Related" 'related))

; numbering is left entirely to CSS counters (scoped to #toc ol / .tr-body,
; see the .taxon.numbered rule a site is expected to define) so it stays in
; sync with the equally CSS-driven numbering in transcluded content, instead
; of being computed here and baked into static text.
(define (recur-toc addr depth)
  (define is-local? (string-contains? addr ":"))
  (define anchor (format "#~a" addr))
  (define page-url
    (cond
      [is-local? anchor]
      [else (addr->url addr)]))
  (define (common-part taxon title entries)
    (li (a 'class: "toc-bullet" 'href: page-url 'target: "_parent" "■")
        (a 'class: "toc-title" 'href: anchor 'target: "_parent"
           (span 'class: "taxon numbered" (if taxon (string-append taxon " ") ""))
           " "
           (literal (or title addr)))
        (unless (= 0 depth)
          (unless (empty? entries)
            (ol
              (for/list ([addr entries])
                (recur-toc addr (sub1 depth))))))))

  (cond
    [is-local?
     ; a local addr has form `addr:count`
     (define tmp (string-split addr ":"))
     (define locals (fetch-metadata (first tmp) 'locals))
     (define idx (string->number (second tmp)))
     (define metadata (list-ref locals idx))
     (common-part (hash-ref metadata 'taxon)
                  (hash-ref metadata 'title)
                  (hash-ref metadata 'transclude '()))]
    [else
     (common-part (fetch-metadata addr 'taxon)
                  (fetch-metadata addr 'title)
                  (fetch-metadata addr 'transclude))]))
(define (generate-toc)
  (define entries (fetch-metadata (self-addr) 'transclude))
  (unless (empty? entries)
    (element 'nav 'id: "toc"
             (a 'class: "toc-top" 'href: "#" 'title: "Back to top" 'aria-label: "Back to top"
                (svg 'class: "toc-top-icon" 'viewBox: "0 0 16 16" 'width: "16" 'height: "16"
                     'fill: "none" 'stroke: "currentColor" 'stroke-width: "1.6"
                     'stroke-linecap: "round" 'stroke-linejoin: "round" 'aria-hidden: "true"
                     (path 'd: "M8 13V4M4 8l4-4 4 4")))
             (h1 "Table of Contents")
             (ol (for/list ([addr entries])
                   (recur-toc addr (sub1 (toc/depth))))))))

(define (tree path)
  (define meta-queue (make-queue))
  (when (fetch-metadata (self-addr) 'date)
    (enqueue! meta-queue (span (fetch-metadata (self-addr) 'date))))
  (define authors
    (for/list ([addr (fetch-metadata (self-addr) 'authors)])
      (a 'class: "link-self" 'href: (string-append "/" addr) (fetch-metadata addr 'title))))
  (define name-authors (fetch-metadata (self-addr) 'name-authors))
  (unless (empty? (append authors name-authors))
    (enqueue! meta-queue (span (add-between (append authors name-authors) ", "))))
  (when (fetch-metadata (self-addr) 'doi)
    (enqueue! meta-queue (span (a 'class: "link-self"
                                  'href: (string-append "https://doi.org/" (fetch-metadata (self-addr) 'doi))
                                  'target: "_blank"
                                  (fetch-metadata (self-addr) 'doi)))))
  (define meta-entries (fetch-metadata (self-addr) 'meta '()))
  (unless (empty? meta-entries)
    (for ([meta meta-entries])
      (enqueue! meta-queue (span meta))))
  (when (fetch-metadata (self-addr) 'orcid)
    (enqueue! meta-queue (span (a 'class: "link-self"
                                  'href: (string-append "https://orcid.org/" (fetch-metadata (self-addr) 'orcid))
                                  'target: "_blank"
                                  (string-append "ORCID: " (fetch-metadata (self-addr) 'orcid))))))
  (define metalink-entries (fetch-metadata (self-addr) 'metalink '()))
  (unless (empty? metalink-entries)
    (for ([metalink metalink-entries])
      (enqueue! meta-queue (span (a 'href: metalink 'target: "_blank" metalink)))))

  (details 'open: #t
           (summary
             (tr-h1 (self-addr) (literal (fetch-metadata (self-addr) 'title)) (fetch-metadata (self-addr) 'taxon))
             (span 'class: "metadata"
                   (add-between (queue->list meta-queue) " · ")))
           (literal (file->string path))))

(define (transclude #:open [open? #t] addr)
  (details 'open: open? 'id: addr
           (summary
             (tr-h1 addr (fetch-metadata addr 'title) (fetch-metadata addr 'taxon) #:numbered? #t)
             (span 'class: "metadata"
                   (span (fetch-metadata addr 'date))
                   (span (fetch-metadata addr 'author))))
           (disable-prefix (file->string (string-append "_tmp/" addr ".embed.html")))))

(define card-counting (make-parameter 0))
(define (tr/card #:open [open? #t] . content)
  (define cc (card-counting))
  (define locals (fetch-metadata (self-addr) 'locals '()))
  (define local-metadata (list-ref locals cc))
  (define title (hash-ref local-metadata 'title))
  (define taxon (hash-ref local-metadata 'taxon))
  (define addr (format "local-~a" cc))
  (define location (format "~a:~a" (self-addr) cc))
  (define link-to-self (a 'class: "link-self" 'href: (string-append "#" location) 'target: "_parent" "[" addr "]"))
  (card-counting (add1 cc))
  (details 'open: open? 'id: location
           (summary
             (h1
               (span 'class: "taxon numbered" (if taxon (string-append taxon " ") ""))
               " "
               title
               " "
               link-to-self))
           (article 'class: "tr-body" content)))

(define (pre* . content)
  (disable-prefix (pre (literal content))))

(define external
  (case-lambda
    [(url) (a 'href: url 'target: "_blank" url)]
    [(url . body) (a 'href: url 'target: "_blank" body)]))

(define mention
  (case-lambda
    [(addr)
     (define url (addr->url addr))
     (a 'class: "mention"
        'target: "_parent"
        'href: url
        (fetch-metadata addr 'title))]
    [(addr . body)
     (define url (addr->url addr))
     (a 'class: "mention"
        'target: "_parent"
        'href: url
        body)]))

(define-syntax note
  (syntax-rules ()
    [(_ item ...) (note* (note-item item) ...)]))

(define-syntax note-item
  (syntax-rules (mention)
    [(_ (mention addr)) (mention-card addr)]
    [(_ other) other]))

(define (note* . content)
  (span 'class: "sidenote-wrap"
        (span 'class: "sidenote-ref" 'tabindex: "0" 'role: "button" 'aria-label: "note")
        (span 'class: "sidenote" content)))

(define (mention-card addr)
  (define m (metadata-store-ref addr))
  (define link
    (a 'class: "mention" 'target: "_parent" 'href: (addr->url addr)
       (or (and m (hash-ref m 'title #f)) addr)))
  (cond
    [(not m) link]
    [else
     (define taxon (hash-ref m 'taxon #f))
     (span 'class: "sn-card"
           (span 'class: "sn-card-head"
                 (when taxon (span 'class: "taxon sn-card-taxon" taxon))
                 (a 'class: "sn-card-title" 'target: "_parent"
                    'href: (addr->url addr) (or (hash-ref m 'title #f) addr)))
           (let ([parts (card-facts m)])
             (unless (null? parts)
               (span 'class: "sn-card-meta" (add-between parts " · ")))))]))

(define (card-facts m)
  (define (author-name a)
    (define am (metadata-store-ref a))
    (if am (hash-ref am 'title a) a))
  (define authors
    (append (for/list ([a (in-list (hash-ref m 'authors '()))]) (author-name a))
            (hash-ref m 'name-authors '())))
  (define date (hash-ref m 'date #f))
  (define doi (hash-ref m 'doi #f))
  (filter values
          (list (and (string? date) date)
                (and (pair? authors) (add-between authors "、"))
                (and doi (a 'href: (string-append "https://doi.org/" doi)
                            'target: "_blank" doi)))))

(define (texfig #:header [header-code ""] . formula)
  (define job-id (symbol->string (gensym 'tex)))
  (define dir (build-path "_tmp" (self-addr)))
  (make-directory* dir)
  (define tex-path (build-path dir (string-append job-id ".tex")))
  (define tex (open-output-file #:exists 'truncate/replace tex-path))
  (displayln "\\documentclass[crop,dvisvgm]{standalone}" tex)
  (displayln header-code tex)
  (displayln "\\begin{document}" tex)
  (for-each (λ (s) (display s tex)) formula)
  (displayln "\n\\end{document}" tex)
  (close-output-port tex)

  (figure (img 'class: "center"
               'src: (string-append "/" (self-addr) "/" job-id ".svg")
               'alt: (string-append "figure " job-id))))

(define (tikzcd . formula)
  (define out (open-output-string))
  (displayln "\\begin{tikzcd}" out)
  (for-each (λ (s) (display s out)) formula)
  (displayln "\\end{tikzcd}" out)
  (apply texfig (list (get-output-string out)) #:header "\\usepackage{quiver}\n"))

(define (typst . formula)
  (define job-id (symbol->string (gensym 'typ)))
  (define dir (build-path "_tmp" (self-addr)))
  (make-directory* dir)
  (define typ-path (build-path dir (string-append job-id ".typ")))
  (define typ (open-output-file #:exists 'truncate/replace typ-path))
  (for-each (λ (s) (display s typ)) formula)
  (close-output-port typ)

  (figure (img 'class: "center"
               'src: (string-append "/" (self-addr) "/" job-id ".svg")
               'alt: (string-append "figure " job-id))))
