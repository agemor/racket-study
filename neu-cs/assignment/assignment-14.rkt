#lang racket


;EXERCISE 1

(define listA (list (list 1 2) (list 2 4) (list 5 4)))

(define (add-up lon lob)
(append lon lob))

; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])

;List-of Lists -> List-of X
(define (together lol)
  (foldr add-up empty lol))

(together listA)

; Exercise #2
(define TEST-LIST (list (list 1 1 3 4) (list 5 5 6 7) (list 10 8 9 10)))

(define (pairwise-disjoint? target-lists)
    (false? (member #false
        (map 
            (lambda (x)
                    (andmap
                        (lambda (y)
                                (if (equal? x y)
                                    #true
                                    (false? (ormap (lambda (z) (member z x)) y))
                                    ))
                    target-lists))
            target-lists)
    ))
)

(pairwise-disjoint? TEST-LIST)

(define-struct book [title author page-count])

; Exercise #3
(define TEST-BOOK-LIST (list (make-book "X" 0 0) (make-book "K" 0 0) (make-book "a" 0 0)))

(define (sort-books book-list)
  (sort book-list (lambda (a b) (string<? (string-downcase (book-title a)) (string-downcase (book-title b))
))))


(map book-title (sort-books TEST-BOOK-LIST))


; Exercise #4
(define (build-identity-matrix n)
  (build-list n (lambda (x) 
                (build-list n (lambda (y) (if (= x y) 1 0))))
))

(build-identity-matrix 10)


; Exercise #5
(define TEST-NUMBER-LIST (list 1 0 0 0 1))

(define (replace-with n target-list) (
    foldr (lambda (a b) (append (if (zero? a) (list n n) (list a)) b)) `() target-list
))

(replace-with 3 TEST-NUMBER-LIST)