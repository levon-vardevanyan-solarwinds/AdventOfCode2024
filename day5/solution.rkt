#lang racket

(require threading)

(define data (map (λ~> (string-split "|") (map string->number _)) (file->lines "rules.txt")))

(define rules (make-hash))

(for-each (λ (i) (hash-update! rules (first i) (λ (x) (set-add x (second i))) (set))) data)

(define updates (map (λ~> (string-split ",") (map string->number _)) (file->lines "input.txt")))

(define (good? lst)
  (let loop ([lst lst]
             [past (set)])
    (if (empty? lst)
        #t
        (if (set-empty? (set-intersect past (hash-ref rules (first lst) (set))))
            (loop (rest lst) (set-add past (first lst)))
            #f))))

(define (middle lst)
  (list-ref lst (quotient (length lst) 2)))

; part one
(apply + (map middle (filter good? updates)))

(define (greater? lhs rhs)
  (set-member? (hash-ref rules lhs (set)) rhs))

; part two
(apply + (map middle (map (curryr sort (negate greater?)) (filter (negate good?) updates))))
