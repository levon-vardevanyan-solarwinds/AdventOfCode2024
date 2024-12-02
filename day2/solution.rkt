#lang racket

(define records
  (for/list ([r (map string-split (file->lines "input.txt"))])
    (map string->number r)))

(define (deltas record)
  (let f ([lst (rest record)]
          [prev (first record)]
          [acc '()])
    (if (empty? lst)
        acc
        (let ([cur (first lst)]) (f (rest lst) cur (cons (- cur prev) acc))))))

(define (bad-level? x)
  (let ([a (abs x)]) (or (< a 1) (> a 3))))

(define (is-delta-valid? lst)
  (and (or (andmap positive? lst) (andmap negative? lst)) (not (memf bad-level? lst))))

(define (is-record-valid? record)
  (is-delta-valid? (deltas record)))

;; part one
(count is-record-valid? records)

(define (gen-off-by1 lst)
  (for/list ([i (in-range (length lst))])
    (for/list ([v lst]
               [j (in-naturals)]
               #:unless (= i j))
      v)))

(define (is-record-valid-tolerance? record)
  (or (is-record-valid? record) (ormap is-record-valid? (gen-off-by1 record))))

;; part two
(count is-record-valid-tolerance? records)
