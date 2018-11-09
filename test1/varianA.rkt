#lang racket/base

(define % modulo)
(define // quotient)

(define (last-digit-of n)
  (% n 10))

(define (remove-last-digit-of n)
  (// n 10))

(define (add-as-last-digit-to digit n)
  (+ (* n 10) digit)
  )

(define (reverse-digits n)
  (define (for current reversed)
    (if [= current 0]
        reversed
        (for (remove-last-digit-of current)
             (add-as-last-digit-to (last-digit-of current)
                                   reversed))
        ))
  (for n 0))

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

(define (apply-n-times n func to)
  (define (for i current)
    (if (= n i)
        current
        (for (+ i 1) (func current)))
    )
  (for 0 to)
  )

(define (permutes-n-times func1 func2 n)
  (= (apply-n-times n (lambda (x) (func1 (func2 x))) n)
     (apply-n-times n (lambda (x) (func2 (func1 x))) n)
     )
  )

;; Task 1 a
(define (diff-reverse n)
  (- n (reverse-digits n)))

;; Task 1 b
(define (sortDigits n)
  (define (for digit current sorted)
    (cond
      [(= digit -1) sorted]
      [(= current 0) (for (- digit 1) n sorted)]
      [else (for digit
              (remove-last-digit-of current)
              (if [= digit (last-digit-of current)]
                  (add-as-last-digit-to digit sorted)
                  sorted
                  ))]
          )
    )
  (for 9 n 0)
  )


;; Task 2

(define (permutable? a b func1 func2)
  (let ([from (+ a (% a 2))])
    (accumulate (lambda (a b) (and a b))
                #t
                from
                b
                (lambda (n) (permutes-n-times func1 func2 n))
                (lambda (x) (+ x 2))
                )
    )
  )


;; Task 3

(define (interval-size interval)
  (- (cdr interval) (car interval)))

(define (bigger-interval interval1 interval2)
  (if [>= (interval-size interval1)
          (interval-size interval2)]
      interval1
      interval2)
  )

(define (subinterval? interval superset)
  (and (<= (car superset)
           (car interval))
       (>= (cdr superset)
           (cdr interval)))
  )

(define (sort_by func to_sort)
  (if [null? to_sort]
      to_sort
      (let* ([head (car to_sort)]
             [rest (cdr to_sort)]
             [front (filter (lambda (x) (func x head )) rest)]
             [back (filter (lambda (x) (not (func x head ))) rest)]
             )
        (append (sort_by func front) (cons head (sort_by func back))))
        ))


(define (longest-interval-subsets intervals)
  (if (null? intervals)
      intervals
      (let* ([longest-interval (foldl bigger-interval
                                      (car intervals)
                                      (cdr intervals))]
             [filter-func (lambda (interval)
                            (subinterval? interval longest-interval))]
             [filtered (filter filter-func intervals)])
        ;; filtered
        (sort_by (lambda (i1 i2) (< (car i1) (car i2))) filtered)
        ))
  )
