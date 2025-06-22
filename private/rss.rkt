#lang racket/base
(provide produce-rss)
(require racket/list
         racket/file
         racket/path)
(require scribble/html/html
         scribble/html/xml)
(require gregor
         dirname)
(require "common.rkt")

(define/provide-elements/not-empty item pubDate description)
(define (produce-rss)
  (define (get-metadata addr)
    (file->json (build-path "_tmp" (string-append addr "." "metadata" ".json"))))
  (define site-obj (file->json "site.json"))
  (define site-url (hash-ref site-obj 'domain))
  (define site-title (hash-ref site-obj 'title))
  (define site-description (hash-ref site-obj 'description))

  (define (itemize items)
    (add-between (for/list ([meta-object items])
                    (define pub-date (iso8601->datetime (hash-ref meta-object 'date)))
                    (item
                      (title (hash-ref meta-object 'title))
                      (link (string-append "https://" (path->string (build-path site-url (hash-ref meta-object 'id)))))
                      (description (literal (file->string (string-append "_tmp/" (hash-ref meta-object 'id) ".embed.html"))))
                      (pubDate (~t pub-date "EEE, dd MMM yyyy HH:mm:ss +0800"))))
                 "\n"))
  (define addrs
    (sort
      (for/list ([path (find-files
                         (lambda (x) (path-has-extension? x #".scrbl"))
                         "content/post")])
        (get-metadata (basename (path-replace-extension path ""))))
      (λ (a b)
        (datetime>=? (iso8601->datetime (hash-ref a 'date)) (iso8601->datetime (hash-ref b 'date))))))

  (define out (open-output-file #:exists 'truncate/replace "_build/rss.xml"))
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
