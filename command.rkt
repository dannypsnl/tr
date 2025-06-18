#lang racket
(require dirname
         json
         file-watchers)
(require "private/next.rkt"
         "private/common.rkt"
         "build.rkt")

(define (find-root-dir dir)
  (if (string=? "/" (path->string dir))
    #f
    (if (directory-exists? (build-path dir "content"))
      dir
      (find-root-dir (string->path (dirname dir))))))

(define (copy-directory-recursively source-dir target-dir)
  (make-directory* target-dir)
  (for ([item (directory-list source-dir)])
    (if (string=? ".git" (path->string item))
      (void)
      (let ([source-path (build-path source-dir item)]
            [target-path (build-path target-dir item)])
        (if (directory-exists? source-path)
            (copy-directory-recursively source-path target-path)
            (copy-file source-path target-path #t))))))

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

  (define root-path (find-root-dir (current-directory)))
  (parameterize ([current-directory (if root-path root-path (current-directory))])
    (match args
      [(list "init")
        (call-with-output-file ".gitignore"
          (λ (out)
            (displayln "_tmp/" out)
            (displayln "_build/" out)
            (displayln "assets/" out)))
        (call-with-output-file "site.json"
          (λ (out)
            (define x (make-hasheq
              '((domain . "your domain")
                (title . "your site title")
                (description . "your site description"))))
            (write-json x out)))
        (system* (find-executable-path "git") "clone" "https://git.sr.ht/~dannypsnl/tr-assets" "assets")
        (system* (find-executable-path "git") "init")
        (make-directory* "content/post")
        (displayln "init done")]
      [(list "watch")
        (unless root-path (raise "You're not in a tr project"))
        (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) "content"))
        (thread-wait
          (watch scrbl-list
            (λ (_paths)
              (search-and-build "content"))
            (λ (_)
              (void))))]
      [(list "build")
        (unless root-path (raise "You're not in a tr project"))
        (define site-obj (file->json "site.json"))
        ; If user didn't assign one, use our setup
        (define assets-directories (hash-ref site-obj 'assets '("assets")))
        (for ([path assets-directories])
          (copy-directory-recursively path "_build"))
        (search-and-build "content")]
      [(list "next" addr-prefix)
        (unless root-path (raise "You're not in a tr project"))
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
      [(list cmd _ ...) (printf "Unknown command `~a`~n" cmd)]
      [_ (displayln "No command provided")]
      )))
