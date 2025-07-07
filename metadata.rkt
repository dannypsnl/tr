#lang racket
(provide compute-addr
         compute-metadata
         compute-racket)
(require scribble/reader
         scribble/html/html
         data/queue
         dirname)
(require "private/common.rkt")

(define (execute f)
  (match f
    [`(m ,text) (m text)]
    [`(mm ,text) (mm text)]
    [`(code ,text) (code text)]
    [t t]))

(define (compute-metadata addr addr-path)
  (define self-title (make-parameter #f))
  (define self-taxon (make-parameter #f))
  (define self-date (make-parameter #f))
  (define self-doi (make-parameter #f))
  (define self-orcid (make-parameter #f))
  (define self-toc/depth (make-parameter #f))

  (define literal-author-queue (make-queue))
  (define author-queue (make-queue))

  (define related-queue (make-queue))
  (define transclude-queue (make-queue))
  (define content-queue (make-queue))
  (define meta-queue (make-queue))
  (define metalink-queue (make-queue))

  (define (handle-form form)
    (match form
      [`(title ,@forms) (self-title (for/list ([f forms]) (execute f)))]
      [`(taxon ,text) (self-taxon text)]
      [`(date ,text) (self-date text)]
      [`(doi ,text) (self-doi text)]
      [`(orcid ,text) (self-orcid text)]
      [`(author ,addr) (enqueue! author-queue addr)]
      [`(author/literal ,name) (enqueue! literal-author-queue name)]
      [`(transclude ,addr) (enqueue! transclude-queue addr)]
      [`(transclude ,@_ ,addr) (enqueue! transclude-queue addr)]
      [`(mention ,addr) (enqueue! related-queue addr)]
      [`(mention ,@_ ,addr) (enqueue! related-queue addr)]
      [`(meta/text ,@forms) (enqueue! meta-queue (for/list ([f forms]) (execute f)))]
      [`(meta/link ,@forms) (enqueue! metalink-queue (for/list ([f forms]) (execute f)))]
      [`(bibtex ,_text) (void)]
      [`(toc/depth ,num) (self-toc/depth num)]
      [`(tr/code ,form) (void)]
      [`(tr/code ,@forms) (void)]
      [t #:when (string? t)
        (enqueue! content-queue t)]
      [`(,_ ,@forms)
        (for ([form forms])
          (handle-form form))]
      [_ (void)]))

  (define forms
    (call-with-input-file addr-path
      (lambda (in) (read-inside in))))
  (for ([form forms])
    (handle-form form))

  (define taxon (self-taxon))
  (define title (self-title))

  (define collected-text (string-join (queue->list content-queue) " "))

  (make-immutable-hash (list
    (cons 'id addr)
    (cons 'date (self-date))
    (cons 'doi (self-doi))
    (cons 'orcid (self-orcid))
    (cons 'authors (queue->list author-queue))
    (cons 'name-authors (queue->list literal-author-queue))
    (cons 'title title)
    (cons 'taxon taxon)
    (cons 'text collected-text)
    (cons 'toc/depth (self-toc/depth))
    ; a list of addresses, later we should update context of these addresses
    (cons 'transclude (queue->list transclude-queue))
    ; a list of addresses, later we should split some of them to references, by checking taxon
    (cons 'related (queue->list related-queue))
    ; general metadata entries (text)
    (cons 'meta (queue->list meta-queue))
    ; external link metadata entries
    (cons 'metalink (queue->list metalink-queue)))))

(define (compute-racket addr-path)
  (define forms
    (call-with-input-file addr-path
      (lambda (in) (read-inside in))))
  (define content-queue (make-queue))
  (for ([form forms])
    (match form
      [`(tr/code ,text) (enqueue! content-queue text)]
      [`(tr/code ,@lst)
        (for ([text lst])
          (enqueue! content-queue text))]
      [_ (void)]))
  (queue->list content-queue))

(define (compute-addr path)
  (basename (path-replace-extension path "")))
