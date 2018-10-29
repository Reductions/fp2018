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
        (let* ([last-digit (last-digit-of current)]
               [next (remove-last-digit-of current)]
               [new-reversed (add-as-last-digit-to
                              last-digit
                              reversed)]
               )
          (do-reverse next new-reversed)
          )))

    (do-reverse n 0))

(define [add-as-last-digit-to digit number]
  (+ (* number 10)
     digit))

(define [last-digit-of n]
  (% n 10))

(define [remove-last-digit-of n]
  (// n 10))



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

;; Task 7
(define [divisors-sum n]
  (let ([half-n (+ (// n 2) 1)]
        )
    (define [for i sum]
      (if [= i half-n]
          sum
          (for (++ i) (if [divides? i n]
                          (+ sum i)
                          sum))))
    (for 1 0)))


(define-test-suite test-devisor-sum
  [test-case "of 1"
    (check-equal? (divisors-sum 1) 0)]
  [test-case "of prime is 1"
    (check-equal? (divisors-sum 7) 1)]
  [test-case "of 26"
    (check-equal? (divisors-sum 28) 28)]
  )

;; Task 8
(define [perfect? n]
  (= n (divisors-sum n)))

(define-test-suite test-perfect?
  [test-case "primes are not perfect"
    (check-false (perfect? 2))
    (check-false (perfect? 3))
    (check-false (perfect? 5))
    (check-false (perfect? 111))]
  [test-case "the first 4 perfect numbers"
    (check-true (perfect? 6))
    (check-true (perfect? 28))
    (check-true (perfect? 496))
    (check-true (perfect? 8128))]
  [test-case "1 is not perfect"
    (check-false (perfect? 1))]
  )

;;Task 9

(define [prime? n]
  (define [for i]
    (cond
      [(= i n) #t]
      [(divides? i n) #f]
      [else (for (+ i 2))]
      ))
  (cond
    [(= n 1) #f]
    [(= n 2) #t]
    [(divides? 2 n) #f]
    [else (for 3)]
    ))

(define-test-suite test-prime?
  [test-case "2 is prime"
    (check-true (prime? 2))]
  [test-case "1 is not prime"
    (check-false (prime? 1))]
  [test-case "evens are not primes"
    (check-false (prime? 4))
    (check-false (prime? 6))
    (check-false (prime? 28))
    (check-false (prime? 496))
    (check-false (prime? 8128))]
  [test-case "first 10 odd primes"
    (check-true (prime? 3))
    (check-true (prime? 5))
    (check-true (prime? 7))
    (check-true (prime? 11))
    (check-true (prime? 13))
    (check-true (prime? 17))
    (check-true (prime? 19))
    (check-true (prime? 23))
    (check-true (prime? 29))
    (check-true (prime? 31))]
  [test-case "first 5 odd compounds"
    (check-false (prime? 9))
    (check-false (prime? 15))
    (check-false (prime? 21))
    (check-false (prime? 25))
    (check-false (prime? 27))]
  )

;;Task 10

(define [increasing? n]
  (define [for left prev-last]
    (cond
      [(= left 0) #t]
      [(>= (last-digit-of left) prev-last) #f]
      [else (for (remove-last-digit-of left) (last-digit-of left))]
      ))

  ;; Избирам 10 за начало защото няма цифра по-голяма от 10
  (for n 10))

(define-test-suite test-increasing?
  [test-case "single digit is always increasing"
    (check-true (increasing? 1))
    (check-true (increasing? 3))
    (check-true (increasing? 8))
    (check-true (increasing? 0))]
  [test-case "some tests with bigger increasing numbers"
    (check-true (increasing? 28))
    (check-true (increasing? 469))
    (check-true (increasing? 1238))]
  [test-case "test with bigger non increasing numbers"
    (check-false (increasing? 82))
    (check-false (increasing? 964))
    (check-false (increasing? 8123))])

;;Task 11

(define [to-binary n]
  (define [for shift number bin]
    (if (= number 0)
        bin
        (let* ([new-shift (* shift 10)]
               [new-number (// number 2)]
               [next-bin-digit (% number 2)]
               [new-bin (+ (* next-bin-digit shift)
                           bin)]
              )
          (for new-shift new-number new-bin)))
    )
  (for 1 n 0))

(define-test-suite test-to-binary
  [test-case "0"
    (check-equal? (to-binary 0) 0)]
  [test-case "1"
    (check-equal? (to-binary 1) 1)]
  [test-case "12345"
    (check-equal? (to-binary 12345) 11000000111001)]
  [test-case "930303"
    (check-equal? (to-binary 930303) 11100011000111111111)]
  )


;;Task 11

(define [to-decimal n]
  (define [for shift number dec]
    (if (= number 0)
        dec
        (let* ([new-shift (* shift 2)]
               [new-number (// number 10)]
               [next-bin-digit (% number 10)]
               [new-dec (+ (* next-bin-digit shift)
                           dec)]
               )
          (for new-shift new-number new-dec)))
    )
  (for 1 n 0))

(define-test-suite test-to-decimal
  [test-case "0"
    (check-equal? (to-decimal 0) 0)]
  [test-case "1"
    (check-equal? (to-decimal 1) 1)]
  [test-case "12345"
    (check-equal? (to-decimal 11000000111001)  12345)]
  [test-case "930303"
    (check-equal? (to-decimal 11100011000111111111) 930303)]
  )

(module+ test
  (require rackunit/text-ui)
  (run-tests test-fast-exp)
  (run-tests test-roots-count)
  (run-tests test-fact)
  (run-tests test-fib)
  (run-tests test-revers-nat)
  (run-tests test-palindrom?)
  (run-tests test-devisor-sum)
  (run-tests test-perfect?)
  (run-tests test-prime?)
  (run-tests test-increasing?)
  (run-tests test-to-binary)
  (run-tests test-to-decimal)
  )
