#lang racket

(require threading)

(define data (file->lines "input.txt"))

(define N (length data))

(define antennas (make-hash))

(for ([line data]
      [i (in-naturals)])
  (for ([ch line]
        [j (in-naturals)])
    (unless (char=? ch #\.)
      (hash-update! antennas ch (λ (x) (cons (list i j) x)) '()))))

(define (in-bounds? lst)
  (andmap (λ (val) (and (>= val 0) (< val N))) lst))

(define (part1 a b)
  (let ([delta (map - b a)])
    (~> (list (map - a delta) (map + b delta))
        (filter in-bounds? _)
        list->set)))

(define (antinodes all for-two)
  (for/fold ([result (set)]) ([ab (in-combinations all 2)])
    (set-union result (apply for-two ab))))

(define (solve for-two)
  (set-count (for/fold ([result (set)]) ([a (in-hash-values antennas)])
               (set-union result (antinodes a for-two)))))

; part one
(solve part1)

(define (part2 a b)
  (define delta (map - b a))
  (define (it cur op [result '()])
    (if (in-bounds? cur)
        (let ([next (map op cur delta)]) (it next op (cons cur result)))
        result))
  (list->set (append (it a -) (it b +))))

; part two
(solve part2)
