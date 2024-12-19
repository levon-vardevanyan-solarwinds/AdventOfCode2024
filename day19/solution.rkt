#lang racket

(require threading)

(define-values (patterns towels)
  (call-with-input-file "input.txt"
                        (λ (in)
                          (let ([patterns (string-split (read-line in) #px"[\\ ,]" #:repeat? #t)])
                            (read-line in)
                            (values patterns
                                    (let loop ([result '()])
                                      (define line (read-line in))
                                      (if (eof-object? line)
                                          (reverse result)
                                          (loop (cons line result)))))))))

(define memo-brute
  (let ([cache (make-hash)])
    (define (brute towel)
      (define cached (hash-ref cache towel 'miss))
      (cond
        [(zero? (string-length towel)) 1]
        [(eq? cached 'miss)
         (let ([result (for/sum ((pattern patterns) #:when (string-prefix? towel pattern))
                                (brute (substring towel (string-length pattern))))])
           (hash-set! cache towel result)
           result)]
        [else cached]))
    brute))

; part one
(for/sum ([towel towels] #:when (positive? (memo-brute towel))) 1)

; part two
(for/sum ([towel towels]) (memo-brute towel))
