#lang racket
(provide compute-metadata)
(require scribble/reader
         data/queue)
(require "private/common.rkt")

(define (compute-metadata addr addr-path)
  (define self-title (make-parameter #f))
  (define (set-self-title . forms)
    (self-title forms))
  (define self-taxon (make-parameter #f))
  (define (set-self-taxon t)
    (self-taxon t))
  (define self-date (make-parameter #f))

  (define literal-author-queue (make-queue))
  (define author-queue (make-queue))
  (define (add-literal-author name)
    (enqueue! literal-author-queue name))
  (define (add-author addr)
    (enqueue! author-queue addr))

  (define related-queue (make-queue))
  (define transclude-queue (make-queue))
  (define content-queue (make-queue))

  (define (handle-form form)
    (match form
      [`(title ,@forms) (set-self-title forms)]
      [`(taxon ,text) (set-self-taxon text)]
      [`(author ,addr) (enqueue! author-queue addr)]
      [`(author/literal ,name) (enqueue! literal-author-queue name)]
      [`(transclude ,addr) (enqueue! transclude-queue addr)]
      [`(mention ,addr) (enqueue! related-queue addr)]
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
    (cons 'authors (queue->list author-queue))
    (cons 'name-authors (queue->list literal-author-queue))
    (cons 'title title)
    (cons 'taxon taxon)
    (cons 'text collected-text)
    ; a list of addresses, later we should update context of these addresses
    (cons 'transclude (queue->list transclude-queue))
    ; a list of addresses, later we should split some of them to references, by checking taxon
    (cons 'related (queue->list related-queue)))))
