#lang racket

(require threading)

(define (parse-register line)
  (match-define (list (list register value))
    (regexp-match* #px"^Register\\ (.):\\ (\\d+)$" line #:match-select cdr))
  (cons register (string->number value)))

(define-values (registers program)
  (call-with-input-file "input.txt"
    (λ (in)
      (let ([registers
             (make-hash
              (list (parse-register (read-line in))
                    (parse-register (read-line in))
                    (parse-register (read-line in))))])
        (read-line in)
        (values registers
                (~> (read-line in)
                    (string-split ":")
                    second
                    (string-split #px"[\\ ,]")
                    (map string->number _)
                    (list->vector)))))))

(define (combo x registers)
  (match x
    [(or 0 1 2 3) x]
    [4 (hash-ref registers "A")]
    [5 (hash-ref registers "B")]
    [6 (hash-ref registers "C")]
    [7 (error "Combo operand 7 is reserved")]))

(struct vm (ip regs prog screen) #:transparent)

(define (jump-to machine)
  (match-define (vm ip regs prog screen) machine)
  (define len (vector-length prog))
  (define next (+ ip 2))
  (if (< ip len)
      (let* ([opcode (vector-ref prog ip)]
             [literal-op (vector-ref prog (add1 ip))]
             [combo-op (combo literal-op regs)]
             [adv-store-at (λ (reg)
                             (hash-set! regs reg (quotient (hash-ref regs "A") (expt 2 combo-op))))])
        (match opcode
          [0 (adv-store-at "A")]
          [1 (hash-set! regs "B" (bitwise-xor (hash-ref regs "B") literal-op))]
          [2 (hash-set! regs "B" (modulo combo-op 8))]
          [3 (unless (zero? (hash-ref regs "A")) (set! next literal-op))]
          [4 (hash-set! regs "B" (bitwise-xor (hash-ref regs "B") (hash-ref regs "C")))]
          [5 (set! screen (cons (modulo combo-op 8) screen))]
          [6 (adv-store-at "B")]
          [7 (adv-store-at "C")])
        (jump-to (vm next regs prog screen)))
      machine))

(define screen-output
  (λ~> vm-screen
       reverse
       (map number->string _)
       (string-join ",")))

; part one
(screen-output (jump-to (vm 0 registers program '())))

(define (run-with-A A) (screen-output (jump-to (vm 0 (make-hash (list (cons "A" A) (cons "B" 0) (cons "C" 0))) program '()))))

(define goal "2,4,1,3,7,5,0,3,4,3,1,5,5,5,3,0")

(define starts-with-2
  (for/set ([A (in-range #b1000000000000)]
            #:when (string-prefix? (run-with-A A) (substring goal 0 1)))
    (bitwise-and A #b111)))

(define (loop [previous starts-with-2] [bits 3] [dpref 2])
  (if (> bits (* (length (string-split goal ",")) 3))
      previous
      (loop
       (for*/set ([low previous]
                  [A (in-range #b1000000000000)]
                  #:when (let ([A (bitwise-ior (arithmetic-shift A bits) low)]
                               [prefix (if (> dpref (string-length goal)) goal (substring goal 0 (+ 1 dpref)))])
                           (string-prefix? (run-with-A A) prefix)))
         (bitwise-and (bitwise-ior (arithmetic-shift A bits) low) (sub1 (expt 2 (+ bits 3)))))
       (+ bits 3)
       (+ dpref 2))))

; part two
(~>
 (loop)
 set->list
 (apply min _))

