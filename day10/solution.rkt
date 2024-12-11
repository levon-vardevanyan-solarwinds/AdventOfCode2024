#lang racket

(require threading)

(define rows (file->lines "input.txt"))

(define N (length rows))
(define M (string-length (first rows)))

(define matrix (make-hash))

(for ([row rows]
      [i (in-naturals)])
  (for ([ch row]
        [j (in-naturals)])
    (hash-set! matrix (cons i j) ch)))

(define starts
  (for*/list ([i (in-range N)]
              [j (in-range M)]
              #:when (char=? #\0 (hash-ref matrix (cons i j))))
    (cons i j)))

(define (out-of-bounds? cur)
  (let ([i (car cur)]
        [j (cdr cur)])
    (or (negative? i) (negative? j) (>= i N) (>= j M))))

(define in-bounds? (negate out-of-bounds?))

(define (where-to from)
  (let* ([i (car from)]
         [j (cdr from)])
    (filter in-bounds?
            (list (cons (sub1 i) j)     ; up
                  (cons (add1 i) j)     ; down
                  (cons i (sub1 j))     ; left
                  (cons i (add1 j)))))) ; down
  
(define (list-summits cursor)
  (define val (hash-ref matrix cursor))
  (if (char=? #\9 val)
      (set cursor)
      (for/fold ([result (set)])
                ([option (where-to cursor)]
                 #:when (= 1
                           (let ([next (hash-ref matrix option)])
                             (- (char->integer next) (char->integer val)))))
        (set-union result (list-summits option)))))

; part one
(apply + (map (λ~> list-summits set-count) starts))

(define (count-hikes cursor)
  (define val (hash-ref matrix cursor))
  (if (char=? #\9 val)
      1
      (for/sum  ([option (where-to cursor)]
                 #:when (= 1
                           (let ([next (hash-ref matrix option)])
                             (- (char->integer next) (char->integer val)))))
        (count-hikes option))))

;part two
(apply + (map count-hikes starts))