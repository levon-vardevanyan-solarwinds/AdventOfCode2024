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
                           (cons i (add1 j)))))))) ; right

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

(define (solve with)
  (for/sum ([island (islands)])
    (* (set-count island)
       (for/sum ([cell island])
         (with cell)))))

; part one
(solve count-fence)

; nw  n  ne
;  w  o   e
; sw  s  se
(struct win (nw n ne w o e sw s se) #:transparent)

(define (mask from)
  (define current (hash-ref matrix from))
  (match-define (cons i j) from)
  (define (same? pos)
    (and (in-bounds? pos)
         (char=? current (hash-ref matrix pos))))
  (apply win
         (map same?
              (list (cons (sub1 i) (sub1 j))    ; nw
                    (cons (sub1 i) j)           ; n
                    (cons (sub1 i) (add1 j))    ; ne
                    (cons i (sub1 j))           ; w
                    from                        ; o
                    (cons i (add1 j))           ; e
                    (cons (add1 i) (sub1 j))    ; sw
                    (cons (add1 i) j)           ; s
                    (cons (add1 i) (add1 j))    ; se
                    ))))

(define (corners-count from)
  (let ([m (mask from)])
    (count identity
           (list
            ; outside
            (nor (win-n m) (win-e m))
            (nor (win-e m) (win-s m))
            (nor (win-s m) (win-w m))
            (nor (win-w m) (win-n m))
            ; inside
            (and (win-n m) (win-e m) (not (win-ne m)))
            (and (win-e m) (win-s m) (not (win-se m)))
            (and (win-s m) (win-w m) (not (win-sw m)))
            (and (win-w m) (win-n m) (not (win-nw m)))))))
  
; part two
(solve corners-count)
