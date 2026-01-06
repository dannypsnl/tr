#lang racket
(require dirname
         json
         file-watchers)
(require "private/next.rkt"
         "private/config.rkt"
         "build.rkt"
         "metadata.rkt")
(require racket/logging
         racket/random)

(define (find-root-dir dir)
  (cond
    [(string=? "/" (path->string dir)) #f]
    [(directory-exists? (build-path dir "content")) dir]
    [else (find-root-dir (string->path (dirname dir)))]))

(define (copy-directory-recursively source-dir target-dir)
  (make-directory* target-dir)
  (for ([item (directory-list source-dir)]
        #:unless (string=? ".git" (path->string item)))
    (define source-path (build-path source-dir item))
    (define target-path (build-path target-dir item))
    (if (directory-exists? source-path)
        (copy-directory-recursively source-path target-path)
        (copy-file source-path target-path #t))))

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
(define config-path "site.json")

(define (run-tr-build)
  (command-line
   #:program "tr build"
   #:usage-help "build tr-notes project"
   #:once-each
   [("-c" "--config") config "Use not default configuration" (set! config-path config)]
   #:args _
   (unless root-path (raise "You're not in a tr project"))
   (setup-config! config-path)
   ; If user didn't assign one, use our setup
   (define assets-directories (get-config 'assets '("assets")))
   (for ([path assets-directories])
     (copy-directory-recursively path (get-output-path)))
   (search-and-build "content")))
(define (run-tr-watch)
  (command-line
   #:program "tr watch"
   #:usage-help "watch and rebuild tr-notes project"
   #:once-each
   [("-c" "--config") config "Use not default configuration" (set! config-path config)]
   #:args _
   (unless root-path (raise "You're not in a tr project"))
   (setup-config! config-path)
   (define scrbl-list (find-files (lambda (x) (path-has-extension? x #".scrbl")) "content"))
   (thread-wait
    (watch scrbl-list
           (λ (_paths)
             (search-and-build "content"))
           (λ (_)
             (void))))))

(define (compute-next-addr prefix random?)
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
  (define max-num (apply max -1 numbers))
  (define numbers-set (list->set numbers))
  (define suffix
    (cond
      [random?
       (int->base36
        (random-ref (sequence-filter (lambda (x) (not (set-member? numbers-set x))) (in-inclusive-range 0 1679615))))]
      ; usual mode: compute new max number
      [else (int->base36 (add1 max-num))]))
  (cond
    [(non-empty-string? prefix) (string-append prefix "-" suffix)]
    [else suffix]))
(define (run-tr-next)
  (define random? #f)

  (command-line
   #:program "tr next"
   #:usage-help "compute next address for <prefix>"
   #:once-each
   [("--random") "Use not default configuration" (set! random? #t)]
   #:args (prefix)
   (unless root-path (raise "You're not in a tr project"))

   (displayln (compute-next-addr prefix random?))))

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
