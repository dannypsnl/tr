#lang racket/base
(provide item title link description pubDate guid
         content-encoded cdata
         create-feed)
(require racket/list)
(require scribble/html/html
         scribble/html/xml)

(define/provide-elements/not-empty item pubDate description guid)

(define (content-encoded . body)
  (apply element/not-empty "content:encoded" body))

(define (channel . body) (apply element/not-empty "channel" body))

(define (create-feed #:title feed-title #:link feed-link #:description feed-description items)
  (string-append
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
    (xml->string
      (element/not-empty "rss" 'version: "2.0"
                          'xmlns:content: "http://purl.org/rss/1.0/modules/content/"
                          (channel
                            (title feed-title)
                            (link feed-link)
                            (description feed-description)
                            (add-between items "\n"))))))
