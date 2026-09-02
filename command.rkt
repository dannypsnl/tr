#lang racket
(require dirname
         json
         "file-watchers/main.rkt")
(require "private/next.rkt"
         "private/config.rkt"
         "private/build.rkt"
         "private/metadata.rkt"
         "private/metadata-store.rkt")
(require racket/logging
         racket/runtime-path)

(define-runtime-path default-assets-dir "default-assets")

(define (files-in subdir kind)
  (for/list ([f (directory-list (build-path default-assets-dir subdir))])
    (cons (path->string (build-path subdir f)) kind)))
(define default-assets
  (append
    (list (cons "style.css" 'stylesheet)
          ; katex.min.css is the official npm:katex@0.18.1 dist build
          (cons "katex.min.css" 'stylesheet))
    (files-in "fonts" 'font)))

(define (warn-shadowed-asset! name kind own-path)
  (define migrated-name (string-append "custom-" (path->string (file-name-from-path name))))
  (case kind
    [(stylesheet)
     (eprintf
       (string-append
         "tr: ~a is bundled with tr now; the build output uses tr's version instead of ~a, so its customizations won't show up on the site (~a itself is untouched on disk).\n"
         "    To keep them: rename it to ~a, then add to site.rkt's 'head:\n"
         "      (link 'rel: \"stylesheet\" 'href: \"/~a\")\n")
       name own-path own-path migrated-name migrated-name)]
    [(font)
     (eprintf
       (string-append
         "tr: ~a is bundled with tr now; the build output uses tr's version instead of ~a, so its customizations won't show up on the site (~a itself is untouched on disk).\n"
         "    tr's own style.css/katex.min.css only load their bundled font files by these exact names; to use\n"
         "    a different font, override the relevant CSS in a rule added via site.rkt's 'head instead.\n")
       name own-path own-path)]))

(define (install-default-assets! user-assets-directories)
  (for ([entry default-assets])
    (define name (car entry))
    (define kind (cdr entry))
    (define src (build-path default-assets-dir name))
    (define target (build-path (get-output-path) name))
    (make-directory* (path-only target))
    ; warnings
    (for ([dir user-assets-directories]
          #:when (file-exists? (build-path dir name)))
      (define own-path (build-path dir name))
      (unless (equal? (file->bytes own-path) (file->bytes src))
        (warn-shadowed-asset! name kind own-path)))
    (copy-file src target #t)))

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
    (call-with-output-file "site.rkt"
      (λ (out)
        (for ([line '("#lang racket/base"
                      "(require scribble/html)"
                      "(provide site)"
                      ""
                      "(define site"
                      "  (hash 'domain \"your domain\""
                      "        'title \"your site title\""
                      "        'description \"your site description\""
                      "        'head (list)))")])
          (displayln line out))))
    (system* (find-executable-path "git") "clone" "https://repo.dannypsnl.me/tr-notes/tr-assets.git" "assets")
    (system* (find-executable-path "git") "init")
    (make-directory* "content/post")
    (displayln "init done")))

(define root-path (find-root-dir (current-directory)))
; Prefer site.rkt; a lingering site.json is migrated by setup-config!.
(define config-path
  (if (file-exists? "site.rkt") "site.rkt" "site.json"))

(define (run-tr-build)
  (command-line
    #:program "tr build"
    #:usage-help "build tr-notes project"
    #:once-each
    [("-c" "--config") config "Use not default configuration" (set! config-path config)]
    #:args _
    (unless root-path (raise-user-error 'tr "not in a tr project"))
    (setup-config! config-path)

    (define assets-directories (get-assets-path))
    (for ([path assets-directories])
      (copy-directory-recursively path (get-output-path)))
    (install-default-assets! assets-directories)
    (search-and-build "content")))
(define (run-tr-watch)
  (command-line
    #:program "tr watch"
    #:usage-help "watch and rebuild tr-notes project"
    #:once-each
    [("-c" "--config") config "Use not default configuration" (set! config-path config)]
    #:args _
    (unless root-path (raise-user-error 'tr "not in a tr project"))
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
    (for/stream ([path scrbl-list])
      (define b (basename (path-replace-extension path "")))
      (define number-text (string-trim b (string-append prefix "-") #:right? #f))
      (define n (base36->int number-text))
      ; if not a number, we use 0 as value
      (if n n 0)))
  (define suffix
    (cond
      [random? (int->base36 (random-unused-address numbers))]
      ; usual mode: compute new max number
      [else (int->base36 (add1 (for/fold ([m -1]) ([n numbers]) (max m n))))]))
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
    (unless root-path (raise-user-error 'tr "not in a tr project"))

    (displayln (compute-next-addr prefix random?))))

(define (run-tr-meta)
  (define all? #f)
  (command-line
    #:program "tr meta"
    #:usage-help "get metadata of <addr>, or every built card's stored metadata with --all"
    #:once-each
    [("--all") "dump every card's metadata from the last `tr build`'s store, as a JSON array" (set! all? #t)]
    #:args args
    (unless root-path (raise-user-error 'tr "not in a tr project"))
    (cond
      [all?
       (open-metadata-store!)
       (write-json (metadata-store-all))
       (close-metadata-store!)]
      [else
       (define addr (first args))
       (define stored
         (and (file-exists? (build-path "_tmp" "metadata.sqlite3"))
              (begin0
                (metadata-store-ref addr)
                (close-metadata-store!))))
       (cond
         [stored (write-json stored)]
         [else
          (define scrbl-list (find-files (lambda (x) (string=? addr (path->string (path-replace-extension (basename x) "")))) "content"))
          (when (empty? scrbl-list)
            (raise-user-error 'tr "no card named ~a" addr))
          (write-json (compute-metadata addr (read-card-forms (first scrbl-list))))])])))

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
        ["meta" (call-command run-tr-meta)]
        [cmd (eprintf "Unknown command ~a\n" cmd)]))))

(module+ main
  (run-tr))
