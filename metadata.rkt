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
  (define local-cards-queue (make-queue))

  (define (handle-card-form form transclude-queue)
    (match form
      [`(tr/card ,@forms)
       (define local-card-title #f)
       (define local-card-taxon #f)
       (define local-transclude-queue (make-queue))
       (for ([form forms])
         (match form
           ; These four must be take over to make sure metadata is correct
           [`(title ,@forms) (set! local-card-title (for/list ([f forms]) (execute f)))]
           [`(taxon ,text) (set! local-card-taxon text)]
           [`(transclude ,addr) (enqueue! local-transclude-queue addr)]
           [`(transclude ,@_ ,addr) (enqueue! local-transclude-queue addr)]
           [`(tr/card ,@forms)
            (handle-card-form `(tr/card ,@forms) local-transclude-queue)]
           [form (handle-form form)]))
       (enqueue! local-cards-queue
                 (make-immutable-hash (list
                                       (cons 'title local-card-title)
                                       (cons 'taxon local-card-taxon)
                                       (cons 'transclude (queue->list local-transclude-queue)))))

       (enqueue! transclude-queue (format "~a:~a" addr (sub1 (queue-length local-cards-queue))))]
      [t (handle-form t)]))
  (define (handle-form form)
    (match form
      [`(title ,@forms) (self-title (for/list ([f forms]) (execute f)))]
      [`(taxon ,text) (self-taxon text)]
      [`(date ,text) (self-date text)]
      [`(doi ,text) (self-doi text)]
      [`(orcid ,text) (self-orcid text)]
      [`(author ,addr) (enqueue! author-queue addr)]
      [`(author/literal ,name) (enqueue! literal-author-queue name)]
      [`(meta/text ,@forms) (enqueue! meta-queue (for/list ([f forms]) (execute f)))]
      [`(meta/link ,@forms) (enqueue! metalink-queue (for/list ([f forms]) (execute f)))]
      [`(bibtex ,_text) (void)]
      [`(toc/depth ,num) (self-toc/depth num)]
      [`(transclude ,addr) (enqueue! transclude-queue addr)]
      [`(transclude ,@_ ,addr) (enqueue! transclude-queue addr)]
      [`(mention ,addr) (enqueue! related-queue addr)]
      [`(mention ,addr ,@_) (enqueue! related-queue addr)]
      [`(tr/code ,form) (void)]
      [`(tr/code ,@forms) (void)]
      [`(tr/card ,@forms) (handle-card-form `(tr/card ,@forms) transclude-queue)]
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
                        ; metadata entry: text
                        (cons 'meta (queue->list meta-queue))
                        ; metadata entry: external link
                        (cons 'metalink (queue->list metalink-queue))
                        (cons 'locals (queue->list local-cards-queue))
                        )))

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
