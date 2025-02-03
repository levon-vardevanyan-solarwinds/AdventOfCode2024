#lang racket

(require threading)

(define rows (file->lines "map.txt"))

(define N (length rows))
(define M (string-length (first rows)))
(define matrix (make-hash))
(define current (cons 0 0))

(define (at pos) (hash-ref matrix pos))
(define (set-at pos val) (hash-set! matrix pos val))

; fill the map
(for ([row rows]
      [i (in-naturals)])
  (for ([ch row]
        [j (in-naturals)])
    (hash-set! matrix (cons i j) ch)))

; find starting position
(set! current
      (for*/first ([i (in-range N)]
                   [j (in-range M)]
                   #:when (char=? #\@ (hash-ref matrix (cons i j))))
        (cons i j)))

; remove player from the map
(set-at current #\.)

; aux visual function
(define (print-matrix)
  (for ([i (in-range N)])
    (for ([j (in-range M)])
      (define pos (cons i j))
      (if (equal? pos current)
          (display "@")
          (display (at (cons i j)))))
    (displayln "")))

(define (out-of-bounds? cur)
  (let ([x (car cur)]
        [y (cdr cur)])
    (or (negative? x) (negative? y) (>= x N) (>= y M))))

(define (move pos dir)
  (match-define (cons i j) pos)
  (match dir
    [#\^ (cons (sub1 i) j)]
    [#\v (cons (add1 i) j)]
    [#\< (cons i (sub1 j))]
    [#\> (cons i (add1 j))]))

; move player if possible
(define (try-move dir)
  (match-define (cons i j) current)
  (define step (move current dir))
  (unless (out-of-bounds? step)
    (define val (at step))
    (match val
      [#\. (set! current step)]
      [#\O (try-push step dir)]
      [#\# (void)])))

; push the box chain and move player if possible
(define (try-push pos dir)
  (define chain-end
    (for/fold ([result pos])
              ([i (in-naturals)]
               #:break (not (char=? (at result) #\O)))
      (move result dir)))
  (when (char=? (at chain-end) #\.)
    (set-at pos #\.)
    (set-at chain-end #\O)
    (set! current pos)))

(define moves
  {~> (file->lines "input.txt")
      (map string-trim _)
      (apply string-append _)})


(for ([m moves]) (try-move m))

(define (GPS pos) (+ (* 100 (car pos)) (cdr pos)))

; part one
(for/sum ([(pos val) (in-hash matrix)]
          #:when (char=? val #\O))
  (GPS pos))
