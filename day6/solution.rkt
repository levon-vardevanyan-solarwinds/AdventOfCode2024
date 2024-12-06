#lang racket

(define rows (file->lines "input.txt"))

(define N (length rows))
(define M (string-length (first rows)))

(define matrix (make-hash))

(for ([row rows]
      [i (in-naturals)])
  (for ([ch row]
        [j (in-naturals)])
    (hash-set! matrix (cons i j) ch)))

(define start
  (for*/first ([i (in-range N)]
               [j (in-range M)]
               #:when (char=? #\^ (hash-ref matrix (cons i j))))
    (cons i j)))

(define (move cur dir)
  (let ([x (car cur)]
        [y (cdr cur)])
    (match dir
      ['up (cons (sub1 x) y)]
      ['down (cons (add1 x) y)]
      ['left (cons x (sub1 y))]
      ['right (cons x (add1 y))])))

(define (out-of-bounds? cur)
  (let ([x (car cur)]
        [y (cdr cur)])
    (or (negative? x) (negative? y) (>= x N) (>= y M))))

(define (wall? mx cur)
  (char=? #\# (hash-ref mx cur)))

(define (turn-cw dir)
  (define dirs (list 'up 'right 'down 'left))
  (define index (add1 (index-of dirs dir)))
  (list-ref dirs (modulo index (length dirs))))

(define (path mx cur dir past)
  (let* ([step (move cur dir)]
         [past (set-add past cur)])
    (cond
      [(out-of-bounds? step) past]
      [(wall? mx step) (path mx cur (turn-cw dir) past)]
      [else (path mx step dir past)])))

(define stepped (path matrix start 'up (set)))

; part one
(set-count stepped)

(define (in-a-loop? mx cur dir past)
  (let* ([tup (list cur dir)]
         [step (move cur dir)]
         [loop (set-member? past tup)]
         [past (set-add past tup)])
    (cond
      [(out-of-bounds? step) #f]
      [loop #t]
      [(wall? mx step) (in-a-loop? mx cur (turn-cw dir) past)]
      [else (in-a-loop? mx step dir past)])))

;; part two
(for/sum ([cell stepped] #:when (let ([matrix (hash-copy matrix)])
                                  (hash-set! matrix cell #\#)
                                  (in-a-loop? matrix start 'up (set))))
         1)
