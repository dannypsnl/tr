#lang racket
(provide base36->int int->base36)

(define alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define base (string-length alphabet))

(define (base36->int text)
  (define len (string-length text))
  (let loop ([sum 0]
             [place 1]
             [r (sub1 len)])
    (if (< r 0)
        sum
        (let* ([x (string-ref text r)]
               [digit-value (string-find alphabet (build-string 1 (λ (_) x)))])
          (if digit-value
            (loop (+ sum (* place digit-value)) (* place base) (sub1 r))
            #f)))))

(define (int->base36 number)  
  (define (convert n acc)
    (match n
      [0 acc]
      [_ (let ([q (quotient n base)]
               [r (remainder n base)])
           (convert q (string-append (string (string-ref alphabet r)) acc)))]))

  (define (pad-zeros str len)
    (let ([str-len (string-length str)])
      (if (< str-len len)
          (string-append (make-string (- len str-len) #\0) str)
          str)))

  (pad-zeros (convert number "") 4))

(module+ test
  (require rackunit)

  (check-false (base36->int ",,,"))

  (check-equal? (int->base36 10) "000A")
  (check-equal? (int->base36 35) "000Z")
  (check-equal? (int->base36 36) "0010")
  (check-equal? (int->base36 1000) "00RS")
  (check-equal? (int->base36 0) "0000")
  (check-equal? (int->base36 123456) "2N9C")
  (check-equal? (int->base36 (base36->int "ZZZZ")) "ZZZZ"))
