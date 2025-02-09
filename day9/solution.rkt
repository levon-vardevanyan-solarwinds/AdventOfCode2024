#lang racket

(require threading)

(define line (first (file->lines "input.txt")))

(define (char->number ch)
  (- (char->integer ch) (char->integer #\0)))
(define (number->char n)
  (integer->char (+ n (char->integer #\0))))

(define (expand-record line)
  (define record (~> line string->list (map char->number _)))
  (list->vector (for/fold ([result '()])
                          ([n record]
                           [i (in-naturals)])
                  (append result
                          (build-list n
                                      (if (even? i)
                                          (const (number->char (quotient i 2)))
                                          (const #\.)))))))

(define (member-in-range vec r pred?)
  (for/first ([i r]
              #:when (pred? (vector-ref vec i)))
    i))

(define (free-space? ch)
  (char=? ch #\.))
(define occupied? (negate free-space?))

(define (swap vec a b)
  (define temp (vector-ref vec a))
  (vector-set! vec a (vector-ref vec b))
  (vector-set! vec b temp))

(define (part1 str)
  (define vec (expand-record str))
  (define len (vector-length vec))
  (define (solve [left 0] [right (sub1 len)])
    (let ([left (member-in-range vec (in-range left len) free-space?)]
          [right (member-in-range vec (in-inclusive-range right 0 -1) occupied?)])
      (when (and left right (< left right))
        (swap vec left right)
        (solve (add1 left) (sub1 right)))))
  (solve)
  (for/sum ([ch vec] [i (in-naturals)] #:when (positive? (char->number ch))) (* i (char->number ch))))

; part one
(part1 line)

(define record (~> line string->list (map char->number _)))

(struct block (pos len id) #:transparent)

(define-values (files gaps)
  (for/fold ([files '()]
             [gaps '()]
             [pos 0]
             #:result (values files (reverse gaps)))
            ([n record]
             [i (in-naturals)])
    (values (if (even? i)
                (cons (block pos n (quotient i 2)) files)
                files)
            (if (odd? i)
                (cons (cons pos n) gaps)
                gaps)
            (+ pos n))))

(define moved '())
(define left '())

(for ([f files])
  (define new-place
    (index-where gaps (λ (g) (and (>= (cdr g) (block-len f)) (> (block-pos f) (car g))))))
  (when new-place
    (set! moved (cons (struct-copy block f [pos (car (list-ref gaps new-place))]) moved))
    (set!
     gaps
     (list-update gaps new-place (λ (g) (cons (+ (car g) (block-len f)) (- (cdr g) (block-len f)))))))
  (unless new-place
    (set! left (cons f left))))

(define (checksum x)
  (for/sum ([i (block-len x)]) (* (block-id x) (+ i (block-pos x)))))

; part two
(apply + (map checksum (append moved left)))
