#lang racket

(require threading)

(define connections (make-hash))

(define (conn-add lhs rhs)
  (hash-update! connections lhs (λ (x) (set-add x rhs)) (set)))

(for ([c (~> (file->lines "input.txt") (map (λ~> (string-split "-")) _))])
  (match-define (list left right) c)
  (conn-add left right)
  (conn-add right left))

(define (inter-connected? computers)
  (for/and ([two (in-combinations (set->list computers) 2)])
    (match-define (list a b) two)
    (set-member? (hash-ref connections a) b)))

(define (at-least-one-starts-with-t? computers)
  (for/or ([c computers])
    (string-prefix? c "t")))

(define (solve)
  (define checked (mutable-set))
  (define connected (list))

  (for ([(from to) (in-hash connections)])
    (for ([comb (in-combinations (set->list to) 2)])
      (define to-check (list->set (cons from comb)))
      (unless (set-member? checked to-check)
        (when (inter-connected? to-check)
          (set! connected (cons to-check connected)))
        (set-add! checked to-check))))

  (count at-least-one-starts-with-t? connected))

; part one
(solve)

(define parties
  (for/fold ([parties (map set (hash-keys connections))])
            ([current (in-hash-keys connections)])
    (for/set ([p parties])
      (define candidate (set-add p current))
      (if (inter-connected? candidate) candidate p))))

; part two
(~> parties
    set->list
    (argmax set-count _)
    set->list
    (sort string<?)
    (string-join ","))
