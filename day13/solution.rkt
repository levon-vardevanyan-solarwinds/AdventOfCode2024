#lang racket

(require threading)
; system of linear equations
(struct sole (Ax Ay Bx By [X #:mutable] [Y #:mutable]) #:transparent)

(define (parse-line regex line)
  (~> line (regexp-match* regex _ #:match-select cdr) first (map string->number _)))

(define parse-button (curry parse-line #px"^Button\\ .:.X([+-]\\d+),\\ Y([+-]\\d+)$"))
(define parse-prize (curry parse-line #px"^Prize:\\ X=(\\d+),\\ Y=(\\d+)$"))

(define (import-record port)
  (match-let ([(list Ax Ay) (parse-button (read-line port))]
              [(list Bx By) (parse-button (read-line port))]
              [(list X Y) (parse-prize (read-line port))])
    (sole Ax Ay Bx By X Y)))

(define (import-all)
  (define infile (open-input-file "input.txt"))
  (define result '())
  (let loop ()
    (set! result (cons (import-record infile) result))
    (unless (eof-object? (read-line infile))
      (loop)))
  (close-input-port infile)
  result)

(define (solve-linear-system s)
  (define B
    (/ (- (* (sole-Y s) (sole-Ax s)) (* (sole-X s) (sole-Ay s)))
       (- (* (sole-Ax s) (sole-By s)) (* (sole-Ay s) (sole-Bx s)))))
  (if (exact-integer? B)
      (let ([A (/ (- (sole-X s) (* B (sole-Bx s))) (sole-Ax s))])
        (if (exact-integer? A)
            (list A B)
            #f))
      #f))

(define (score AB)
  (+ (* (first AB) 3) (second AB)))

(define data (import-all))

(define (solve)
  (apply + (map score (filter identity (map solve-linear-system data)))))

; part one
(solve)

(for ([s data])
  (set-sole-X! s (+ (sole-X s) 10000000000000))
  (set-sole-Y! s (+ (sole-Y s) 10000000000000)))

; part two
(solve)
