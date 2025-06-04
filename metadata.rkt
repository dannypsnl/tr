#lang racket
(provide
  generate-metadata
  (rename-out [set-self-title title]
              [set-self-taxon taxon]
              [self-date date]
              [add-author author]
              [add-literal-author author/literal]
              [collect-text p]
              [collect-text li]
              [collect-text code]
              [collect-text pre]
              [collect-text tikzcd])
  m mm
  transclude
  mention
  (except-out (all-from-out scribble/html/html)
    p li code pre title))
(require scribble/html/html
         scribble/html/xml)
(require json data/queue)
(require "private/common.rkt")

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

(define (collect-text . content)
  (for ([t content]
        #:when (string? t))
    (enqueue! content-queue t)))

(define (mention addr . _)
  (enqueue! related-queue addr))

(define (transclude addr)
  (enqueue! transclude-queue addr))

(define (generate-metadata)
  (define addr (self-addr))
  (define taxon (self-taxon))
  (define title (self-title))

  (define collected-text (string-join (queue->list content-queue) " "))

  (define metadata
    (make-hasheq (list
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

  (define out (open-output-file #:exists 'replace (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (write-json	metadata out)
  (close-output-port out))
