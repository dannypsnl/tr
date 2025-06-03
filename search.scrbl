#lang scribble/text
@(require racket/file
          racket/path)
@(define (itemize items)
  (add-between (map (lambda (item) @file->string{@item})
                    items)
               ","))
@(define json-list (find-files (lambda (x) (path-has-extension? x #".metadata.json")) "_tmp"))
[
  @itemize[@json-list]
]
