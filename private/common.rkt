#lang racket
(provide
 file->json json->file
 non-local?
 m mm)
(require json
         racket/runtime-path)

(define (file->json path)
  (define in (open-input-file path))
  (define obj (read-json in))
  (close-input-port in)
  obj)
(define (json->file json path)
  (define out (open-output-file #:exists 'truncate/replace path))
  (write-json json out)
  (close-output-port out))

(define-runtime-path katex-stdio-deno "../katex-stdio-deno.ts")

(define katex-worker-lock (make-semaphore 1))
(define katex-worker-stdout #f)
(define katex-worker-stdin #f)
(define katex-worker-ctrl #f)

(define (start-katex-worker!)
  (match-define (list out in _pid _err ctrl)
    (process* (find-executable-path "deno") "run" katex-stdio-deno))
  (set! katex-worker-stdout out)
  (set! katex-worker-stdin in)
  (set! katex-worker-ctrl ctrl)
  (plumber-add-flush! (current-plumber)
                      (lambda (_) (ctrl 'kill))))

(define (read-until-null in)
  (define out (open-output-bytes))
  (let loop ()
    (define b (read-byte in))
    (cond
      [(eof-object? b) (error 'read-until-null "eof before sentinel")]
      [(= b 0) (bytes->string/utf-8 (get-output-bytes out))]
      [else (write-byte b out) (loop)])))

(define (latex->html tex #:display? [display? #f])
  (call-with-semaphore
   katex-worker-lock
   (lambda ()
     (unless katex-worker-ctrl (start-katex-worker!))
     (fprintf katex-worker-stdin "~a~a\n" (if display? "d" "i") tex)
     (flush-output katex-worker-stdin)
     (read-until-null katex-worker-stdout))))

(define (m formula) (latex->html formula))
(define (mm . forms)
  (define formula (apply string-append forms))
  (latex->html formula #:display? #t))

(define (non-local? addr)
  (not (string-contains? addr ":")))
