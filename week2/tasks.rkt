#lang racket/base

(require "../prelude.rkt")

(require rackunit)

;; Task 1

(define [fast-exp x n]
  (define [square x] (* x x))
  (cond
    [(= n 0) 1]
    [(< n 0) (/ 1 (fast-exp x (- n)))]
    [(even? n) (square (fast-exp x (/ n 2)))]
    [#t (* x (fast-exp x (-- n)))]
    ))


(define-test-suite test-fast-exp
  [test-case "can raise to positive even power"
    (check-equal? (fast-exp 2 4) (expt 2 4))
    (check-equal? (fast-exp -2 4) (expt -2 4))
    (check-= (fast-exp 2.3 4) (expt 2.3 4) eps)
    (check-equal? (fast-exp 1/3 4) (expt 1/3 4))
    ]

  [test-case "can raise to positive odd power"
    (check-equal? (fast-exp 2 5) (expt 2 5))
    (check-equal? (fast-exp -4 5) (expt -4 5))
    (check-= (fast-exp 2.3 5) (expt 2.3 5) eps)
    (check-equal? (fast-exp 1/3 5) (expt 1/3 5))
    ]

  [test-case "can raise to negative even power"
    (check-equal? (fast-exp 2 -4) (expt 2 -4))
    (check-equal? (fast-exp -4 -4) (expt -4 -4))
    (check-= (fast-exp 2.3 -4) (expt 2.3 -4) eps)
    (check-equal? (fast-exp 1/3 -4) (expt 1/3 -4))
    ]

  [test-case "can raise to negative odd power"
    (check-equal? (fast-exp 2 -5) (expt 2 -5))
    (check-equal? (fast-exp -4 -5) (expt -4 -5))
    (check-= (fast-exp 2.3 -5) (expt 2.3 -5) eps)
    (check-equal? (fast-exp 1/3 -5) (expt 1/3 -5))
    ]

  [test-case "can raise to 0"
    (check-equal? (fast-exp 2 0) (expt 2 0))
    (check-equal? (fast-exp -4 0) (expt -4 0))
    (check-= (fast-exp 2.3 0) (expt 2.3 0) eps)
    (check-equal? (fast-exp 1/3 0) (expt 1/3 0))
    ]
  )

;; Task 2

(define [roots-count a b c]
  (let*
      ([b^2 (* b b)]
       [4ac (* 4 a c)]
       [D (- b^2 4ac)])
    (cond
      [(and (= a 0)
            (= b 0)
            (= c 0)) 'inf]
      [(and (= a 0)
            (= b 0)) 0]
      [(= a 0) 1]
      [(= D 0) 1]
      [(< D 0) 0]
      [else 2]
      )))

(define-test-suite test-roots-count
  [test-case "a = 0, b = 0, c = 0"
    (check-equal? (roots-count 0 0 0) 'inf)]
  [test-case "a = 0, b = 0, c <> 0"
    (check-equal? (roots-count 0 0 1) 0)]
  [test-case "a = 0, b <> 0, c <> 0"
    (check-equal? (roots-count 0 1 1) 1)]
  [test-case "D < 0"
    (check-equal? (roots-count 1 1 1) 0)]
  [test-case "D = 0"
    (check-equal? (roots-count 1 2 1) 1)]
  [test-case "D > 0"
    (check-equal? (roots-count 1 3 1) 2)]
  )

;; Task 3

(define [fact n]
  (define [for i acc]
    (if (> i n)
        acc
        (for (++ i) (* acc i))))

  (for 2 1))

(define-test-suite test-fact
  [test-case "0!"
    (check-equal? (fact 0) 1)]
  [test-case "1!"
    (check-equal? (fact 1) 1)]
  [test-case "8!"
    (check-equal? (fact 5) 120)]
  )

;; Task 4

(define [fib n]
  (define [for i prev curr]
    (if (= i n)
        curr
        (for (++ i) curr (+ prev curr))))

  (for 1 0 1))

(define-test-suite test-fib
  [test-case "F(1)"
    (check-equal? (fib 1) 1)]
  [test-case "F(2)"
    (check-equal? (fib 2) 1)]
  [test-case "F(5)"
    (check-equal? (fib 5) 5)]
  [test-case "F(13)"
    (check-equal? (fib 13) 233)]
  )

;; Task 5

(define [reverse-nat n]
  (define [do-reverse current reversed]
    (if (equal? current 0)
        reversed
        (let* ([last-digit (% current 10)]
               [next (// current 10)]
               [new-reversed (+ (* reversed 10)
                                last-digit)]
               )
          (do-reverse next new-reversed)
          )))

    (do-reverse n 0))


(define-test-suite test-revers-nat
 [test-case "can reverse one digit"
   (check-equal? (reverse-nat 7) 7)]
 [test-case "can reverse big int"
   (check-equal? (reverse-nat 72) 27)]
 [test-case "can reverse 0"
   (check-equal? (reverse-nat 0) 0)]
 )


;; Task 6
(define [palindrom? n]
  (equal? n (reverse-nat n)))

(define-test-suite test-palindrom?
  [test-case "1 digite is plindrom"
    (check-true (palindrom? 7))]
  [test-case "123 is not plindrom"
    (check-false (palindrom? 123))]
  [test-case "1 digite is plindrom"
    (check-true (palindrom? 123454321))]
  )

(module+ test
  (require rackunit/text-ui)
  (run-tests test-fast-exp)
  (run-tests test-roots-count)
  (run-tests test-fact)
  (run-tests test-fib)
  (run-tests test-revers-nat)
  (run-tests test-palindrom?)
  )
