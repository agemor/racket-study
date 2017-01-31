#lang racket

;; Recursion Practice: Fibonacci Sequence
;; 1 1 2 3 5 8 11 ...
;; A(1) = 1
;; A(2) = 1
;; A(n) = A(n-1) + A(n-2)

;; A(5) = A(4) -> A3 + A2 + A3



;; fibonacci
;; n(integer) -> N(integer)
;; n is the n-th element of the sequence
(define (fibonacci n)
  (cond [(= n 1) 1]
       [(= n 2) 1]
       [else (+ (fibonacci (- n 1)) (fibonacci (- n 2))) ]))

;; 1! = 1
;; 2! = 2
;; 3! = 6
;; 4! = 24

;; factorial n
;; n!
;; A(n) = n * A(n-1)
;; A(1) = 1

(define (factorial n)
  (cond [(=  n 1) 1]
            [else (* n (factorial (- n 1)))]))


;; 5x2
;; 5x9

(define (gu-gu-dan start)
      (cond [(= start 10) 1]
           [else
              (printf (number->string (* start 5)))
              (gu-gu-dan (+ start 1))]))

(gu-gu-dan 2)
(number->string (fibonacci 15)))