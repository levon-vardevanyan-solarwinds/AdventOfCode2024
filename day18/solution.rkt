#lang racket

(require threading)

(define lines (file->lines "input.txt"))
(define fallen 1024)
(define N 70)
(define start (cons 0 0))
(define finish (cons N N))

(define (invert lst)
  (cons (second lst) (first lst)))

(define line->posn (λ~> (string-split ",") (map string->number _) invert))

(define corrupted (apply mutable-set (map line->posn (take lines fallen))))

(define (is-corrupted? pos)
  (set-member? corrupted pos))

(define not-corrupted? (negate is-corrupted?))

(define (out-of-bounds? pos)
  (match-define (cons i j) pos)
  (or (negative? i) (negative? j) (> i N) (> j N)))

(define in-bounds? (negate out-of-bounds?))

(define (where-to from)
  (match-define (cons i j) from)
  (filter (λ (x) (and (in-bounds? x) (not-corrupted? x)))
          (list (cons (sub1 i) j) ; up
                (cons (add1 i) j) ; down
                (cons i (sub1 j)) ; left
                (cons i (add1 j)) ; right
                )))

(struct cell (pos from) #:transparent)

(define (bfs [queue (list (cell start null))] [track (hash)] [visited (set)])
  (if (empty? queue)
      (hash) ; unreachable
      (match-let ([(cell pos from) (first queue)])
        (if (equal? pos finish)
            (hash-set track pos from) ; done
            (if (set-member? visited pos)
                (bfs (rest queue) track visited) ; skip - already visited
                (let ([next-cells (for/list ([next-pos (where-to pos)])
                                    (cell next-pos pos))])
                  (bfs (append (rest queue) next-cells)
                       (hash-set track pos from)
                       (set-add visited pos))))))))

(define (backtrack from)
  (for/fold ([current finish]
             [result (list)]
             #:result result)
            ([i (in-naturals)]
             #:break (equal? current start))
    (values (hash-ref from current) (cons current result))))

; part one
(length (backtrack (bfs)))

(set-clear! corrupted)

; part two
(for/last ([line lines]
           #:break (hash-empty? (bfs)))
  (set-add! corrupted (line->posn line))
  line)
