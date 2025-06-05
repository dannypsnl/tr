#lang racket
(require dirname
         file-watchers)
(require "private/next.rkt"
         "build.rkt")

(define (find-root-dir dir)
  (if (string=? "/" (path->string dir))
    #f
    (if (directory-exists? (build-path dir "content"))
      dir
      (find-root-dir (string->path (dirname dir))))))

(module+ main
  (define args
    (let ([dst-dir (make-parameter "_build")])
      (command-line
       #:once-each
       ["--dest" file
                 "destination directory of build, default is `_build`"
                 (dst-dir (string->path file))]
       #:usage-help
       "tr command"
       #:args args
       args)))

  (parameterize ([current-directory (find-root-dir (current-directory))])
    (match args
      [(list "watch")
        (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) "content"))
        (thread-wait
          (watch scrbl-list
            (λ (_paths)
              (search-and-build "content"))
            (λ (_)
              (void))))]
      [(list "build") (search-and-build "content")]
      [(list "next" addr-prefix)
        (define scrbl-list
          (find-files
            (λ (path)
              (and (path-has-extension? path #".scrbl")
                   (string-prefix? (basename path) addr-prefix)))
            "content"))
        (define numbers
          (for/list ([path scrbl-list])
            (define b (basename (path-replace-extension path "")))
            (define number-text (string-trim b (string-append addr-prefix "-") #:right? #f))
            (define n (base36->int number-text))
            ; if not a number, we use 0 as value
            (if n n 0)))
        (define max-num (apply max (cons -1 numbers)))
        (displayln (string-append addr-prefix "-" (int->base36 (add1 max-num))))]
      [_ (println "Unknown command")])))
