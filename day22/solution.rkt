#lang racket

(require threading data/ring-buffer)

(define (mix value secret)
  (bitwise-xor value secret))

(define (prune value)
  (modulo value 16777216))

(define (phase-1 secret)
  (~> secret
      (* 64)
      (mix secret)
      prune))

(define (phase-2 secret)
  (~> secret
      (/ 32)
      floor
      (mix secret)
      prune))

(define (phase-3 secret)
  (~> secret
      (* 2048)
      (mix secret)
      prune))

(define (next-secret current)
  (~> current
      phase-1
      phase-2
      phase-3))

(define (gen secret n)
  (for/fold ([secret secret])
            ([i (in-range n)])
    (next-secret secret)))

(define secrets (map string->number (file->lines "input.txt")))

; part one
(apply + (map (curryr gen 2000) secrets))

(define (digit x) (remainder x 10))

; delta is in range [-9, 9]
; -9 -> 0, ... , 0 -> 9, ... , 9 -> 18
(define (delta->code x) (+ x 9))
(define (code->delta x) (- x 9))

; four consecutive changes coded as AABBCCDD 
(define (four-changes->code deltas)
  (for/fold ([result 0])
            ([delta deltas]
             [power (in-inclusive-range 6 0 -2)])
    (+ result (* (delta->code delta) (expt 10 power)))))

(define (code->four-changes code)
  (for/list ([power (in-inclusive-range 6 0 -2)])
    (code->delta (remainder (quotient code (expt 10 power)) 100))))

(define gens 2000)

; cost sequences e.g. for 123: 3 0 6 5 4 4 6 4 4 2 ...
; offset map "four changes code" -> cost sequences vector index
(struct line (cost offset) #:transparent) 

(define (generate start gens)
  (define sliding-window (empty-ring-buffer 4))
  (define delta-map (make-hash))

  (line
   (for/fold ([current start][result (make-vector (sub1 gens))] #:result result)
             ([i (in-range (sub1 gens))])
     (define next (next-secret current))
     (define delta (- (digit next) (digit current)))
     (vector-set! result i (digit next))
     (ring-buffer-push! sliding-window delta)
     (define window (for/list ([v sliding-window]) v))
     (when (= (length window) 4)
       (define code (four-changes->code window))
       (define entry (hash-ref delta-map code #f))
       (unless entry (hash-set! delta-map code i)))
     (values next result))
   delta-map))

(define all-seqs (mutable-set))

(define database
  (for/vector ([secret secrets])
    (let ([l (generate secret gens)])
      (set-union! all-seqs (list->set (hash-keys (line-offset l))))
      l)))

(define (count-bananas seq)
  (for/sum ([l database])
    (define index (hash-ref (line-offset l) seq -1))
    (if (= index -1) 0 (vector-ref (line-cost l) index))))

; part two
(for/fold ([max-bananas 0])
          ([seq all-seqs])
  (max (count-bananas seq) max-bananas))
