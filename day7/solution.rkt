#lang racket

(require threading)

(define data
  (for/list ([line (map (λ (x) (string-split x ":")) (file->lines "input.txt"))])
    (list (string->number (first line)) (map string->number (string-split (second line))))))

(define ops1 (list + *))

(define (combs n ops)
  (apply cartesian-product (build-list n (λ (_) ops))))

(define (f-apply lst fs)
  (for/fold ([acc (first lst)])
                  ([x (rest lst)] [f fs])
    (f acc x)))

(define (solve ops)
  (for/sum ([line data])
    (let ([lhs (first line)]
          [rhs (second line)])
      (if (for/first ([comb (combs (length rhs) ops)]
                      #:when (= lhs (f-apply rhs comb)))
            #t) lhs 0))))

; part one
(solve ops1)

(define (concat lhs rhs)
  (string->number (format "~a~a" lhs rhs)))

(define ops2 (list + * concat))

; part two
(solve ops2)