#lang scribble/text
@(require racket/file
          racket/path
          json
          dirname
          "private/common.rkt")
@(require scribble/html/html
          scribble/html/extra
          scribble/html/xml)
@(define (get-metadata addr)
  (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
@(define site-obj (file->json "site.json"))
@(define site-url (hash-ref site-obj 'domain))
@(define site-title (hash-ref site-obj 'title))
@(define site-description (hash-ref site-obj 'description))

@(define/provide-elements/not-empty item)

@(define (itemize items)
  (add-between (for/list ([addr items])
                  (define meta-object (get-metadata addr))
                  (item
                    (title (hash-ref meta-object 'title))
                    (link (format "https://~a~a" site-url addr))))
               "\n"))

@(define addrs
  (for/list ([path (find-files
                     (lambda (x) (path-has-extension? x #".scrbl"))
                     "content/post")])
    (basename (path-replace-extension path ""))))

<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0">
<channel>
  <title>@site-title </title>
  <link>@site-url </link>
  <description>@site-description </description>

  @itemize[@addrs]
</channel>
</rss>
