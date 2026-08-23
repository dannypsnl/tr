#lang racket
(provide open-metadata-store!
         close-metadata-store!
         metadata-store-ref
         metadata-store-set!
         with-metadata-transaction)
(require db json)

(define current-conn (box #f))

(define (store-path tmp-dir) (build-path tmp-dir "metadata.sqlite3"))

; opened/closed per call, not a long-lived singleton, so a wiped/recreated project dir can't leave a stale connection
(define (open-metadata-store! [tmp-dir (build-path "_tmp")])
  (close-metadata-store!)
  (define conn (sqlite3-connect #:database (store-path tmp-dir) #:mode 'create))
  (set-box! current-conn conn)
  (query-exec conn "CREATE TABLE IF NOT EXISTS metadata (addr TEXT PRIMARY KEY, json TEXT NOT NULL)")
  conn)

(define (close-metadata-store!)
  (define conn (unbox current-conn))
  (when conn (disconnect conn))
  (set-box! current-conn #f))

(define (conn!)
  (or (unbox current-conn)
      (open-metadata-store!)))

(define (metadata-store-ref addr)
  (define row (query-maybe-row (conn!) "SELECT json FROM metadata WHERE addr = ?" addr))
  (and row
       (with-handlers ([exn:fail? (lambda (e) (eprintf "warning: corrupt metadata for ~a: ~a~n" addr (exn-message e)) #f)])
         (define obj (string->jsexpr (vector-ref row 0)))
         (and (jsexpr? obj) obj))))

(define (metadata-store-set! addr obj)
  (query-exec
    (conn!)
    "INSERT INTO metadata(addr, json) VALUES (?, ?) ON CONFLICT(addr) DO UPDATE SET json = excluded.json"
    addr (jsexpr->string obj)))

(define (with-metadata-transaction thunk)
  (call-with-transaction (conn!) thunk))
