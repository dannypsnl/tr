#lang scribble/text
@(require json "private/common.rkt")
@(define (get-metadata addr)
  (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
@(define site-obj (file->json "site.json"))
@(define site-url (hash-ref site-obj 'domain))
@(define site-title (hash-ref site-obj 'title))
@(define site-description (hash-ref site-obj 'description))

@(define (itemize items)
  (add-between (map (lambda (addr)
                      (define meta-object (get-metadata addr))
                      (format "<item>
  <title>~a</title>
  <link>https://~a/~a</link>
</item>"
                      (hash-ref meta-object 'title) site-url addr))
                    items)
               "\n"))

@(define addrs '("0000"))

<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0">
<channel>
  <title>@site-title </title>
  <link>@site-url </link>
  <description>@site-description </description>

  @itemize[@addrs]
</channel>
</rss>
