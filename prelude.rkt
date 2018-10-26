#lang racket/base

(provide ++ -- % // natural? devides? eps)

(require rackunit)
(require racket/math)

(define [++ n]
  (+ n 1))

(define [-- n]
  (- n 1))

(define % modulo)

(define // quotient)

(define (natural? n)
  (and (integer? n)
       (>= n 0)))

(define [devides? divisor dividend]
  (= (% dividend divisor) 0))

(define eps (expt 0.1 6))

(module+ test
  ; Testing ++
  [test-case "Can increment positiv"
    (check-equal? (++ 2) 3)]

  [test-case "Can increment negativ"
    (check-equal? (++ -1) 0)]

  [test-case "Can increment fraction"
    (check-equal? (++ 1/2) 3/2)]

  [test-case "Can decrement floating pointer"
    (check-equal? (++ 1.5) 2.5)]

  ; Testing --
  [test-case "Can decrement positiv"
    (check-equal? (-- 2) 1)]

  [test-case "Can decrement negative"
    (check-equal? (-- -1) -2)]

  [test-case "Can decrement fraction"
    (check-equal? (-- 1/2) -1/2)]

  [test-case "Can decrement floating pointer"
    (check-equal? (-- 1.5) 0.5)]
  )
