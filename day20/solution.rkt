#lang racket

(require threading data/queue memo)

(define rows (file->lines "input.txt"))

(define N (length rows))
(define M (string-length (first rows)))

(define matrix (make-hash))
(define nodes (mutable-set))

(define start null)
(define finish null)

(for ([(row i) (in-indexed rows)])
  (for ([(ch j) (in-indexed row)])
    (match ch
      [#\S (set! start (cons i j))
           (set-add! nodes (cons i j))]
      [#\E (set! finish (cons i j))
           (set-add! nodes (cons i j))]
      [#\. (set-add! nodes (cons i j))]
      [else (void)])
    (hash-set! matrix (cons i j) ch)))

(define (in-bounds? cur)
  (let ([i (car cur)]
        [j (cdr cur)])
    (and (>= i 0) (>= j 0) (< i N) (< j M))))

(define (wall? pos)
  (char=? #\# (hash-ref matrix pos)))

(define/memoize (n-steps-away n)
  (define result '())
  (for ([x (in-inclusive-range (- n) n)])
    (define y (- n (abs x)))
    (set! result (cons (cons x y) result))
    (when (not (zero? y))
      (set! result (cons (cons x (- y)) result))))
  result)

(define (n-steps-away-from from n)
  (match-define (cons x y) from)
  (for/list ([step (n-steps-away n)])
    (cons (+ x (car step)) (+ y (cdr step)))))

(define (where-to from [n 1])
  (filter (conjoin in-bounds? (negate wall?)) (n-steps-away-from from n)))

(define (bfs-shortest-paths from nodes)
  ; BFS queue
  (define q (make-queue))
  (enqueue! q from)
  ; shortest distance from the start node
  (define distance (make-hash))
  (for ([cell nodes])
    (hash-set! distance cell +inf.0))
  (hash-set! distance from 0)
  ; breadcrumb trail to backtrack path to the origin
  (define predec (make-hash))

  (let loop ()
    (if (queue-empty? q)
        distance
        (let* ([current (dequeue! q)]
               [cur-dist (hash-ref distance current)])
          (for ([next (where-to current)])
            (define next-dist (hash-ref distance next))
            (cond
              ; unvisited yet
              [(eq? next-dist +inf.0)
               (enqueue! q next)
               (hash-set! distance next (add1 cur-dist))
               (hash-update! predec next (λ (x) (set-add x current)) (set))]
              ; already visited from a different cell - alternative path found
              [(= next-dist (add1 cur-dist))
               (hash-update! predec next (λ (x) (set-add x current)) (set))]))
          (loop)))))

; apparently we don't need predecessors list for this one
(define dist-from-start (bfs-shortest-paths start nodes))
(define dist-from-finish (bfs-shortest-paths finish nodes))

(define honest (hash-ref dist-from-start finish))

(define (solve rads)
  (define cheats (make-hash))

  (for ([node nodes])
    (define part1 (hash-ref dist-from-start node))
    (for* ([rad rads][jump (where-to node rad)])
      (define part2 (hash-ref dist-from-finish jump))
      (define delta (- honest (+ rad part1 part2)))
      (when (positive? delta)
        (hash-update! cheats delta add1 0))))
  ;(pretty-print cheats)
  (for/sum ([(saved n) (in-hash cheats)] #:when (>= saved 100)) n))

; part one
(solve '(2))

; part two
(solve (inclusive-range 2 20))