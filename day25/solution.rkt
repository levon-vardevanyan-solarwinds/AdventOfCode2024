#lang racket

(require threading)

(define (read-items)
  (define (read-raw from)
    (for/list ([i (in-range 7)])
      (read-line from)))
  (call-with-input-file "input.txt"
    (λ (in)                  
      (let loop ([result '()])
        (define item (read-raw in))
        (if (eof-object? (read-line in))
            (reverse (cons item result))
            (loop (cons item result)))))))

(define (parse-lock item)
  (for/fold ([result (make-list 6 -1)])
            ([line item])
    (for/list ([lhs result][rhs line])
      (+ lhs (if (char=? rhs #\#) 1 0)))))

(define (parse-key item)
  (parse-lock (reverse item)))

(define-values
  (locks keys)
  (for/fold ([locks '()][keys '()]
                        #:result (values (reverse locks) (reverse keys)))
            ([item (read-items)])
    (cond
      [(string=? (first item) "#####") (values (cons (parse-lock item) locks) keys)]
      [(string=? (first item) ".....") (values locks (cons (parse-key item) keys))])))

(define (overlap? lock key)
  (memf (λ (x) (> x 5)) (map + lock key)))

(length
 (for*/list ([lock locks][key keys]
                         #:unless (overlap? lock key))
   (list lock key)))
