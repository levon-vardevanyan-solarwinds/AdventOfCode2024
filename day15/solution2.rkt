#lang racket

(require threading)

;; wide warehouse now!
(define rows
  (map (λ~> (string-replace "#" "##")
            (string-replace "O" "[]")
            (string-replace "." "..")
            (string-replace "@" "@."))
       (file->lines "map.txt")))

(define N (length rows))
(define M (string-length (first rows)))

;; map tiles - walls as well as boxes and floor
(define matrix (make-hash))

;; player position
(define current (cons 0 0))

;; getter/setter for the matrix
(define (at pos) (hash-ref matrix pos))
(define (set-at! pos val) (hash-set! matrix pos val))

;; fill the map
(for ([row rows]
      [i (in-naturals)])
  (for ([ch row]
        [j (in-naturals)])
    (hash-set! matrix (cons i j) ch)))

;; find starting position
(set! current
      (for*/first ([i (in-range N)]
                   [j (in-range M)]
                   #:when (char=? #\@ (hash-ref matrix (cons i j))))
        (cons i j)))

;; remove player from the map
(set-at! current #\.)

;; aux visual function - print current map with player
(define (print-world)
  (for ([i (in-range N)])
    (for ([j (in-range M)])
      (define pos (cons i j))
      (if (equal? pos current)
          (display "@")
          (display (at (cons i j)))))
    (displayln "")))

;; position of the neighbour one step towards direction
(define (neighbour pos dir)
  (match-define (cons i j) pos)
  (match dir
    [#\^ (cons (sub1 i) j)]
    [#\v (cons (add1 i) j)]
    [#\< (cons i (sub1 j))]
    [#\> (cons i (add1 j))]))

(struct box (left right) #:transparent)

(define (box-cells b)
  (list (box-left b) (box-right b)))

;; build the box based on the fragment
(define (build-box pos)
  (match (at pos)
    [#\[ (box pos (neighbour pos #\>))]
    [#\] (box (neighbour pos #\<) pos)]))

;; no checks, just move the box!
(define (move-box b dir)
  (match dir
    [(or #\^ #\v)
     (set-at! (neighbour (box-left b) dir) (at (box-left b)))
     (set-at! (neighbour (box-right b) dir) (at (box-right b)))
     (set-at! (box-left b) #\.)
     (set-at! (box-right b) #\.)]
    [#\<
     (set-at! (neighbour (box-left b) #\<) (at (box-left b)))
     (set-at! (box-left b) (at (box-right b)))
     (set-at! (box-right b) #\.)]
    [#\>
     (set-at! (neighbour (box-right b) #\>) (at (box-right b)))
     (set-at! (box-right b) (at (box-left b)))
     (set-at! (box-left b) #\.)]))

;; move the player if possible, try to push the boxes if needed
(define (try-move-player dir)
  (match-define (cons i j) current)
  (define step (neighbour current dir))
  (match (at step)
    [#\. (set! current step)]
    [(or #\[ #\]) (try-push step dir)]
    [#\# (void)]))

(define (try-push pos dir)
  (define pushed?
    (match dir
      [(or #\< #\>) (push-chain-horizontally (list (build-box pos)) dir)]
      [(or #\^ #\v) (push-layers-vertically (build-box pos) dir)]))
  (when pushed? (set! current pos)))

(define (push-chain-horizontally chain dir)
  (define push-to
    (match dir
      [#\< (neighbour (box-left (first chain)) #\<)]
      [#\> (neighbour (box-right (first chain)) #\>)]))
  (match (at push-to)
    [#\. (for ([b chain]) (move-box b dir)) #t]
    [#\[ (push-chain-horizontally (cons (build-box push-to) chain) #\>)]
    [#\] (push-chain-horizontally (cons (build-box push-to) chain) #\<)]
    [#\# #f]))

(define (push-layers-vertically b dir)
  (let loop ([layers (list (set b))])
    (define push-to
      (for/fold ([result '()])([b (first layers)])
        (append (map (λ (x) (neighbour x dir)) (box-cells b)) result)))
    (cond
      ; if we have at least one block in the chain - nothing moves
      [(findf (λ (x) (char=? (at x) #\#)) push-to) #f]
      ; space is free, we can push the whole group
      [(andmap (λ (x) (char=? (at x) #\.)) push-to)
       (for* ([layer layers][b layer])
         (move-box b dir)) #t]
      ; create another layer of boxes
      [else (loop (cons
                   (for/set ([pos push-to]
                             #:when (let ([ch (at pos)])
                                      (or (char=? #\] ch)
                                          (char=? #\[ ch))))
                     (build-box pos))
                   layers))])))

(define moves
  (~> (file->lines "input.txt")
      (map string-trim _)
      (apply string-append _)))

(for ([m moves]) (try-move-player m))

;(print-world)
   
(define (GPS pos) (+ (* 100 (car pos)) (cdr pos)))
   
; part two
(for/sum ([(pos val) (in-hash matrix)]
          #:when (char=? val #\[))
  (GPS pos))
