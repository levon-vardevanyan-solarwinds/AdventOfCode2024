#lang racket

(define records (map string-split (file->lines "input.txt")))

(define lhs (sort (map string->number (map first records)) <))
(define rhs (sort (map string->number (map second records)) <))

; part one
(apply + (map (λ (a b) (abs (- a b))) lhs rhs))

; part two
(define freq (make-hash))
(for ([i rhs]) (hash-update! freq i add1 0))
(apply + (map (λ (x) (* x (hash-ref freq x 0))) lhs))