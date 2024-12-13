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

(define all-points
  (for*/set ([i (in-range N)]
             [j (in-range M)])
    (cons i j)))

(define (out-of-bounds? cur)
  (let ([i (car cur)]
        [j (cdr cur)])
    (or (negative? i) (negative? j) (>= i N) (>= j M))))

(define in-bounds? (negate out-of-bounds?))

(define (where-to from)
  (let* ([ch (hash-ref matrix from)]
         [i (car from)]
         [j (cdr from)])
    (list->set
     (filter (λ (x) (char=? ch (hash-ref matrix x)))
             (filter in-bounds?
                     (list (cons (sub1 i) j)       ; up
                           (cons (add1 i) j)       ; down
                           (cons i (sub1 j))       ; left
                           (cons i (add1 j)))))))) ; down

(define (flood start)
  (let loop ([current start] [stepped (set)])
    (if (set-member? stepped current)
        stepped
        (for/fold ([result (set-add stepped current)])
                  ([step (where-to current)])
          (set-union result (loop step result))))))
  
(define (islands [points all-points] [result '()])
  (if (set-empty? points)
      result
      (let ([island (flood (set-first points))])
        (islands (set-subtract points island) (cons island result)))))

(define (count-fence current)
  (- 4 (set-count (where-to current))))

; part one
(for/sum ([island (islands)])
  (* (set-count island)
     (for/sum ([cell island])
       (count-fence cell))))
