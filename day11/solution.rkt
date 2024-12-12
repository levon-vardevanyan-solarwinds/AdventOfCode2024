#lang racket

(require threading
         memo)

(define (digits num)
  (~> num (log 10) exact-floor add1))

(define (split num x)
  (define-values (left right) (quotient/remainder num (expt 10 x)))
  (list left right))

(define (process-stone num)
  (if (zero? num)
      '(1)
      (let ([d (digits num)])
        (if (even? d)
            (split num (/ d 2))
            (list (* num 2024))))))

(define/memoize (blink stone n)
                (if (zero? n)
                    1
                    (for/sum ([s (process-stone stone)]) (blink s (sub1 n)))))

(define (solve n)
  (for/sum ([stone start]) (blink stone n)))

(define start '(9759 0 256219 60 1175776 113 6 92833))

; part one
(solve 25)

; part two
(solve 75)
