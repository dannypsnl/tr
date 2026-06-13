#lang racket
(require "../metadata.rkt")

(define temp-dir "/tmp/tr-test-metadata")

(define (setup-test-dir)
  (when (directory-exists? temp-dir)
    (delete-directory/files temp-dir))
  (make-directory* (build-path temp-dir "content")))

(define (cleanup-test-dir)
  (when (directory-exists? temp-dir)
    (delete-directory/files temp-dir)))

(module+ test
  (require rackunit)
  (setup-test-dir)

  (parameterize ([current-directory temp-dir])
    (define test-file (build-path "content" "test.scrbl"))
    (call-with-output-file test-file
      #:exists 'replace
      (lambda (out)
        (displayln "@title{A}" out)
        (displayln "@taxon{Test}" out)
        (displayln "@date{2023-01-01}" out)
        (displayln "@author{test-author}" out)
        (displayln "@p{This is test content.}" out)))

    (check-equal? (compute-addr "content/test.scrbl") "test")

    (define metadata (compute-metadata "test" test-file))
    (check-equal? (hash-ref metadata 'id) "test")
    (check-equal? (hash-ref metadata 'taxon) "Test")
    (check-equal? (hash-ref metadata 'title) '("A"))
    (check-equal? (hash-ref metadata 'date) "2023-01-01")
    (check-equal? (hash-ref metadata 'authors) '("test-author"))
    (check-true (string-contains? (hash-ref metadata 'text) "This is test content"))

    ; @mention/hidden feeds the backlink graph (related) just like @mention,
    ; even though it renders nothing in the body.
    (define hidden-file (build-path "content" "hidden.scrbl"))
    (call-with-output-file hidden-file
      #:exists 'replace
      (lambda (out)
        (displayln "@title{H}" out)
        (displayln "@mention{visible-target}" out)
        (displayln "@mention/hidden{agda-target}" out)
        (displayln "@p{body}" out)))
    (define hidden-meta (compute-metadata "hidden" hidden-file))
    (check-not-false (member "visible-target" (hash-ref hidden-meta 'related)) "plain mention recorded")
    (check-not-false (member "agda-target" (hash-ref hidden-meta 'related)) "hidden mention recorded in related")
    )

  (cleanup-test-dir))
