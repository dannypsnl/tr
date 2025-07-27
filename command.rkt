#lang racket
(require dirname
         json
         file-watchers)
(require "private/next.rkt"
         "private/config.rkt"
         "build.rkt"
         "metadata.rkt")
(require racket/logging)

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

(define (run-tr-init)
  (command-line
    #:program "tr init"
    #:usage-help "setup tr-notes project for beginner"
    #:args _
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
    (displayln "init done")))

(define root-path (find-root-dir (current-directory)))
(define (run-tr-build)
  (command-line
    #:program "tr build"
    #:usage-help "build tr-notes project"
    #:args _
    (unless root-path (raise "You're not in a tr project"))
    (setup-config! "site.json")
    ; If user didn't assign one, use our setup
    (define assets-directories (get-config 'assets '("assets")))
    (for ([path assets-directories])
      (copy-directory-recursively path (get-output-path)))
    (search-and-build "content")))
(define (run-tr-watch)
  (command-line
    #:program "tr watch"
    #:usage-help "watch and rebuild tr-notes project"
    #:args _
    (unless root-path (raise "You're not in a tr project"))
    (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) "content"))
    (thread-wait
      (watch scrbl-list
        (λ (_paths)
          (search-and-build "content"))
        (λ (_)
          (void))))))

(define (run-tr-next)
  (command-line
    #:program "tr next"
    #:usage-help "compute next address for <prefix>"
    #:args (prefix)
    (unless root-path (raise "You're not in a tr project"))
    
    (define scrbl-list
      (find-files
        (λ (path)
          (and (path-has-extension? path #".scrbl")
               (string-prefix? (basename path) prefix)))
        "content"))
    (define numbers
      (for/list ([path scrbl-list])
        (define b (basename (path-replace-extension path "")))
        (define number-text (string-trim b (string-append prefix "-") #:right? #f))
        (define n (base36->int number-text))
        ; if not a number, we use 0 as value
        (if n n 0)))
    (define max-num (apply max (cons -1 numbers)))
    (displayln (string-append prefix "-" (int->base36 (add1 max-num))))))

(define (run-tr-meta)
  (command-line
    #:program "tr meta"
    #:usage-help "get metadata of <addr>"
    #:args (addr)
    (unless root-path (raise "You're not in a tr project"))
    (define scrbl-list (find-files (lambda (x) (string=? addr (path->string (path-replace-extension (basename x) "")))) "content"))
    (write-json (compute-metadata addr (first scrbl-list)))))

(define (run-tr)
  (command-line
   #:program "tr"

   #:usage-help
   "\n<command> is one of

\tinit
\tbuild
\twatch
\tnext
\tmeta

For help on these, use 'build --help', 'next --help', etc."

   #:ps "\nSee https://tr-notes.srht.site/guide-0005 for details."
   #:args (command . leftover-args)
   (define leftover-arg-vector (vector->immutable-vector (list->vector leftover-args)))
   (define (call-command command-thunk)
     (parameterize ([current-command-line-arguments leftover-arg-vector])
       (with-logging-to-port (current-error-port)
         command-thunk
         #:logger (current-logger)
         'info 'tr
         'error)))

   (parameterize ([current-directory (if root-path root-path (current-directory))])
     (match command
       ["init" (call-command run-tr-init)]
       ["build" (call-command run-tr-build)]
       ["watch" (call-command run-tr-watch)]
       ["next" (call-command run-tr-next)]
       ["meta" (call-command run-tr-meta)]))))

(module+ main
  (run-tr))
