#lang racket

(require "../prelude.rkt")

(require rackunit)

;; Task 1
(define (!! n)
  (accumulate * ;; operation
              1 ;; null-value
              (if [divides? 2 n] 2 1) ;; start
              n ;; end
              I ;; transformation
              (lambda (x) (+ x 2)) ;; increment
              ))

;; Task 2

(define (binomial n k)
  (/ (accumulate * 1 (++ k) n I ++)
     (accumulate * 1 1 (- n k) I ++))
  )

;; Task 3

(define (binomial2 n k)
  (define (transformation x)
    (/ (+ k x) x))

  (accumulate * 1 1 (- n k) transformation ++)
  )

;; Task 4

(define (2^ n)
  (accumulate + 0 0 n (lambda (x) (binomial2 n x)) ++)
  )

;; Task 5
;; Вече имаме реализация на тази функция от предходното упражнение
;; Тук ще я напишем с accumulate
(define (divisors-sum n)
  (accumulate +
              0
              1
              (-- n)
              (lambda (x) (if (divides? x n) x 0))
              ++)
  )

;; Task 6

#| Won't do |#

;; Task 7

(define (count p? a b)
  (accumulate +
              0
              a
              b
              (lambda (x) (if (p? x) 1 0))
              ++)
  )

;; Task 8

(define (all p? a b)
  (accumulate (lambda (x1 x2) (and x1 x2))
              #t
              a
              b
              (lambda (x) (p? x))
              ++)
  )

;; Task 9

(define (any p? a b)
  (not (all (lambda (x) (not (p? x))) a b))
  )


;; Task 10

(define (prime? a)
  (not (any (lambda (x) (divides? x a)) 2 (-- a)))
  )
