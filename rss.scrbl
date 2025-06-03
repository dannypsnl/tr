#lang scribble/text
@(require json)
@(define (get-metadata addr)
  (define in (open-input-file (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (read-json in))
@(define site-title "tr")
@(define site-url "https://dannypsnl.github.io/tr")
@(define site-description "tr is a site generator based on a collection of scribbles & racket programs.")

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
