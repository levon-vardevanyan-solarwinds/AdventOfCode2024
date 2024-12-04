#lang racket

(require threading)

(define rows (file->lines "input.txt"))

(define string->vector (λ~> string->list list->vector))

(define matrix (list->vector (map string->vector rows)))

(define N (vector-length matrix))
(define M (~> matrix (vector-ref 0) vector-length))

(define (at i j)
  (~> matrix (vector-ref i) (vector-ref j)))

(define (diags-//)
  (define (f x y)
    (for/list ([i (in-inclusive-range x 0 -1)]
               [j (in-range y M)])
      (at i j)))
  (append (for/list ([i (in-range N)])
            (f i 0))
          (for/list ([j (in-range 1 M)])
            (f (sub1 N) j))))

(define (diags-\\)
  (define (f x y)
    (for/list ([i (in-range x N)]
               [j (in-range y M)])
      (at i j)))
  (append (for/list ([i (in-range N)])
            (f i 0))
          (for/list ([j (in-range 1 M)])
            (f 0 j))))

(define xmas "XMAS")

(define diags
  (~> (append (diags-\\) (diags-//))
      (filter (λ (x) (>= (length x) (string-length xmas))) _)
      (map (λ (x) (apply string x)) _)))

(define cols
  (map list->string
       (for/list ([j (in-range M)])
         (for/list ([i (in-range N)])
           (at i j)))))

(define reverse-string (λ~> string->list reverse list->string))

(define (count-matches str)
  (define (f s n)
    (length (regexp-match* (regexp n) s)))
  (+ (f str xmas) (f str (reverse-string xmas))))

; part one
(apply + (map count-matches (append diags rows cols)))

; part two
(for*/sum ([i (in-range 1 (sub1 N))] [j (in-range 1 (sub1 M))] #:when (char=? #\A (at i j)))
          (let ([d1 (string (at (sub1 i) (sub1 j)) (at (add1 i) (add1 j)))]
                [d2 (string (at (sub1 i) (add1 j)) (at (add1 i) (sub1 j)))])
            (if (and (or (string=? d1 "MS") (string=? d1 "SM"))
                     (or (string=? d2 "MS") (string=? d2 "SM")))
                1
                0)))
