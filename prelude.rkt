#lang racket/base

(provide ++ -- % // natural? divides? eps)

(require rackunit)
(require racket/math)

(define % modulo)
(define // quotient)
(define eps (expt 0.1 6))

(define [++ n]
  (+ n 1))

(define-test-suite test-increment/++
  [test-case "Can increment positiv"
    (check-equal? (++ 2) 3)]

  [test-case "Can increment negativ"
    (check-equal? (++ -1) 0)]

  [test-case "Can increment fraction"
    (check-equal? (++ 1/2) 3/2)]

  [test-case "Can increment floating pointer"
    (check-equal? (++ 1.5) 2.5)]
  )


(define [-- n]
  (- n 1))

(define-test-suite test-decrement/--
  [test-case "Can decrement positiv"
    (check-equal? (-- 2) 1)]

  [test-case "Can decrement negative"
    (check-equal? (-- -1) -2)]

  [test-case "Can decrement fraction"
    (check-equal? (-- 1/2) -1/2)]

  [test-case "Can decrement floating pointer"
    (check-equal? (-- 1.5) 0.5)]
  )

(define [divides? divisor dividend]
  (= (% dividend divisor) 0))

(define-test-suite test-divides?
  [test-case "1 divides all other natural numbers"
    (check-true (divides? 1 2))
    (check-true (divides? 1 13))
    (check-true (divides? 1 1))
    (check-true (divides? 1 3215124))]

  [test-case "natural number divides itself"
    (check-true (divides? 2 2))
    (check-true (divides? 13 13))
    (check-true (divides? 1 1))
    (check-true (divides? 3215124 3215124))]

  [test-case "2 does not divide odd numbers"
    (check-false (divides? 2 13))
    (check-false (divides? 2 25))]

  [test-case "numbers don't divide smaller numbers then them"
    (check-false (divides? 2 13))
    (check-false (divides? 2 25))]
  )

(module+ test
  (require rackunit/text-ui)
  (run-tests test-increment/++)
  (run-tests test-decrement/--)
  (run-tests test-divides?)
  )
