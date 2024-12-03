#lang racket

(define data (file->string "input.txt"))

(define (mul tup)
  (define-values (lhs rhs) (apply values tup))
  (* (string->number lhs) (string->number rhs)))

(define (solve str)
  (define pairs (regexp-match* #px"mul\\((\\d+),(\\d+)\\)" str #:match-select cdr))
  (apply + (map mul pairs)))

;; part one
(solve data)

(set! data (string-append "do()" data "don't()"))

(define matches (regexp-match* #px"do\\(\\).+?don't\\(\\)" data))

;; part two
(apply + (map solve matches))
