#lang racket
(provide open-metadata-store!
         close-metadata-store!
         metadata-store-ref
         metadata-store-set!
         metadata-store-all
         scrbl-marker-hashes
         scrbl-marker-add!
         scrbl-marker-remove!
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
  (query-exec conn "CREATE TABLE IF NOT EXISTS scrbl_marker (addr TEXT NOT NULL, mode TEXT NOT NULL, hash TEXT NOT NULL, PRIMARY KEY (addr, mode, hash))")
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

(define (metadata-store-all)
  (for/list ([row (in-list (query-rows (conn!) "SELECT json FROM metadata ORDER BY addr"))])
    (string->jsexpr (vector-ref row 0))))

(define (scrbl-marker-hashes addr mode)
  (for/list ([row (in-list (query-rows (conn!) "SELECT hash FROM scrbl_marker WHERE addr = ? AND mode = ?" addr mode))])
    (vector-ref row 0)))

(define (scrbl-marker-add! addr mode hash)
  (query-exec
    (conn!)
    "INSERT INTO scrbl_marker(addr, mode, hash) VALUES (?, ?, ?) ON CONFLICT(addr, mode, hash) DO NOTHING"
    addr mode hash))

(define (scrbl-marker-remove! addr mode hash)
  (query-exec (conn!) "DELETE FROM scrbl_marker WHERE addr = ? AND mode = ? AND hash = ?" addr mode hash))

(define (with-metadata-transaction thunk)
  (call-with-transaction (conn!) thunk))
