#lang racket

(require threading)

(define N 101)
(define M 103)
(define seconds 100)

(define (parse-line line)
  (match-define (list (list x y dx dy))
    (regexp-match* #px"^p=(-?\\d+),(-?\\d+)\\ v=(-?\\d+),(-?\\d+)$" line #:match-select cdr))
  (list (map string->number (list x y)) (map string->number (list dx dy))))

(define records (map parse-line (file->lines "input.txt")))

(define (move robot n)
  (match-define (list pos vel) robot)
  (list (map + pos (map (curry * n) vel)) vel))

(define (teleport pos)
  (match-define (list i j) pos)
  (list (modulo i N) (modulo j M)))

(define (run records seconds)
  (~> records
      (map (curryr move seconds) _)
      (map first _)
      (map teleport _)))

(define (quadrants data)
  (define mid-N (quotient N 2))
  (define mid-M (quotient M 2))
  (let loop ([lst data] [result '(0 0 0 0)])
    (if (empty? lst)
        result
        (match-let ([(list i j) (first lst)]
                    [(list a b c d) result])
          (cond [(and (< i mid-N) (< j mid-M)) (loop (rest lst) (list (add1 a) b c d))]
                [(and (> i mid-N) (< j mid-M)) (loop (rest lst) (list a (add1 b) c d))]
                [(and (< i mid-N) (> j mid-M)) (loop (rest lst) (list a b (add1 c) d))]
                [(and (> i mid-N) (> j mid-M)) (loop (rest lst) (list a b c (add1 d)))]
                [else (loop (rest lst) result)])))))

; part one
(apply * (quadrants (run records seconds)))

; part two
(define outfile (open-output-file "tree.txt" #:exists 'replace))

(for ([secs (in-range 10000)])
  (display "\n" outfile)
  (display (number->string secs) outfile)
  (display "\n" outfile)
  (define points (list->set (run records secs)))
  (for ([j (in-range M)])
    (display "\n" outfile)
    (for ([i (in-range N)])
      (display (if (set-member? points (list i j)) "X" ".") outfile))))

(close-output-port outfile)