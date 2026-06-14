#lang racket/base
(provide produce-rss)
(require racket/list
         racket/file
         racket/path)
(require scribble/html/html
         scribble/html/xml)
(require gregor
         dirname)
(require "common.rkt"
         "config.rkt")

(define/provide-elements/not-empty item pubDate description)
(define (produce-rss)
  (define (get-metadata addr)
    (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (define site-obj (file->json "site.json"))
  (define site-url (hash-ref site-obj 'domain))
  (define site-title (hash-ref site-obj 'title))
  (define site-description (hash-ref site-obj 'description))

  ; A post in content/post whose .scrbl omits a `date` line gets 'date => #f in
  ; its metadata. iso8601->datetime then fails with an opaque contract violation
  ; that blames rss.rkt without naming the post. Surface a message that does.
  (define (meta->datetime meta-object)
    (define date (hash-ref meta-object 'date #f))
    (unless (string? date)
      (error 'produce-rss
             "post ~s (id ~s) has no `date`; add a `date` line to its .scrbl so it can appear in the RSS feed"
             (hash-ref meta-object 'title "<untitled>")
             (hash-ref meta-object 'id "<unknown>")))
    (iso8601->datetime date))

  (define (itemize items)
    (add-between (for/list ([meta-object items])
                   (define pub-date (meta->datetime meta-object))
                   (item
                     (title (hash-ref meta-object 'title))
                     (link (string-append "https://" (path->string (build-path site-url (hash-ref meta-object 'id)))))
                     (description (file->string (string-append "_tmp/" (hash-ref meta-object 'id) ".embed.html")))
                     (pubDate (~t pub-date "EEE, dd MMM yyyy HH:mm:ss +0800"))))
                 "\n"))
  (define addrs
    (sort
      (for/list ([path (find-files
                         (lambda (x) (path-has-extension? x #".scrbl"))
                         "content/post")])
        (get-metadata (basename (path-replace-extension path ""))))
      (λ (a b)
        (datetime>=? (meta->datetime a) (meta->datetime b)))))

  (define out (open-output-file #:exists 'truncate/replace (build-path (get-output-path) "rss.xml")))
  (fprintf out "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<rss version=\"2.0\">
<channel>
  <title>~a</title>
  <link>~a</link>
  <description>~a</description>
  ~a
</channel>
</rss>
" site-title
           site-url
           site-description
           (xml->string (itemize addrs)))
  (close-output-port out))
