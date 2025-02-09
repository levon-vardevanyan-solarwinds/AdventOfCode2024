#lang racket

(require data/heap)

(define lines (file->lines "input.txt"))
;; representing two-dimentional maze as a flat vector
;; (i . j) element accessed as maze[i * dimx + j]
(define maze (list->vector (apply append (map string->list lines))))
(define dimx (string-length (first lines))) ; how many columns
(define dimy (length lines)) ; how many rows
(define start (vector-member #\S maze))
(define end (vector-member #\E maze))

; directory is just an index in this array
(define dirs (vector (- dimx) 1 dimx -1))

(define (turn where-to? current)
  (modulo
   (match where-to?
     ['right (add1 current)]
     ['left (sub1 current)])
   4))

; queue helpers
(define (heap<=? lhs rhs)
  (<= (first lhs) (first rhs)))
(define (make-queue)
  (make-heap heap<=?))
(define (enqueue q score pos dir path)
  (heap-add! q (list score pos dir path)))
(define (dequeue q)
  (let ([result (heap-min q)])
    (heap-remove-min! q)
    result))
(define (queue-empty? q)
  (zero? (heap-count q)))

(define (dijkstra)
  (define-values (highscore paths) (values +inf.0 '()))
  ; queue record: '(score position direction path)
  (define q (make-queue))
  (enqueue q 0 start 1 "")
  ; store direction in visited hash!
  ; (position . direction) -> score
  (let loop ([visited (hash)])
    (unless (queue-empty? q)
      (match-define (list score pos d path) (dequeue q))
      (cond
        [(> score highscore) (void)] ; break! cos also true for all remaining in queue
        [(let ([found (hash-ref visited (cons pos d) 'not-found)])
           (and (not (eq? found 'not-found)) (< found score)))
         ; continue! already visited from the same direction and it was cheaper
         (loop visited)]
        [else
         (when (= pos end)
           (set! highscore score)
           (set! paths (cons path paths)))
         (unless (char=? (vector-ref maze (+ pos (vector-ref dirs d))) #\#)
           (enqueue q (add1 score) (+ pos (vector-ref dirs d)) d (string-append path "F")))
         (enqueue q (+ score 1000) pos (turn 'right d) (string-append path "R"))
         (enqueue q (+ score 1000) pos (turn 'left d) (string-append path "L"))
         (loop (hash-set visited (cons pos d) score))])))
  (values highscore paths))

(define-values (highscore paths) (dijkstra))

;; part one
highscore

(define (tiles path)
  (for/fold ([pos start][d 1][stepped (set start)] #:result stepped)
            ([action path])
    (match action
      [#\L (values pos (turn 'left d) stepped)]
      [#\R (values pos (turn 'right d) stepped)]
      [#\F (let ([pos (+ pos (vector-ref dirs d))])
             (values pos d (set-add stepped pos)))])))

;; part two
(set-count (apply set-union (map tiles paths)))