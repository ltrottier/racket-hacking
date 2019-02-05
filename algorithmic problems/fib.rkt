#lang racket


(define (fib n)
  (define (fib_rec n m2 m1)
    (if (<= n 2)
      m1
      (fib_rec (- n 1) m1 (+ m1 m2))))
  (fib_rec n 1 1))
