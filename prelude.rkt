#lang racket/base

(provide ++ -- % //
         natural? divides? eps
         I
         accumulate accumulate-i
         )

(require rackunit)
(require racket/math)

(define % modulo)
(define // quotient)
(define (I x) x)
(define eps (expt 0.1 6))

(define [++ n]
  (+ n 1))

(define [-- n]
  (- n 1))


(define [divides? divisor dividend]
  (= (% dividend divisor) 0))

(define (accumulate operation
                    null-value
                    start
                    end
                    transformation
                    increment)
  (if (> start end)
      null-value
      (operation (transformation start)
                 (accumulate operation
                             null-value
                             (increment start)
                             end
                             transformation
                             increment))
      ))

(define (accumulate-i operation
                      null-value
                      start
                      end
                      transformation
                      increment)
  (if (> start end)
      null-value
      (operation (accumulate-i operation
                             null-value
                             (increment start)
                             end
                             transformation
                             increment)
                 (transformation start))
      ))
