;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;LOS is one of
;empty
;(cons string LOS)

(define los-1 empty)
(define los-2 (cons "happy" (cons "sad" empty)))


;Exercise 1
;LOS->LOS
;
(define (less-than los n)
  (cond [(empty? los) los]
        [(cons? los) (cond [(< (string-length (first los)) n)
                             (cons (first los) (less-than (rest los) n))]
                           [else (less-than(rest los) n)])]))
;(less-than los-2 6)

;Exercies 2
;LOS-> Boolean

(define (all-greater? los n)
  (cond [(empty? los) #false]
        [(cons? los) (less-or-same los n)]))

(define (less-or-same los n)
  (cond [(empty? los) #true]
        [(cons? los) (cond [(<= (string-length (first los)) n) #false]
                           [else (less-or-same (rest los) n)])]))

(all-greater? empty 3)


;---
;(define (less-or-same los n)
;  (cond [(empty? los) los]
;        [(cons? los) (cond [(<= (string-length (first los)) n)
                          ;   (cons (first los) (less-or-same (rest los) n))]
                          ; [else (less-or-same (rest los) n)])]))
;(define (count-number los)
;  (cond [(empty? los) 0]
;        [(cons? los) (+ 1 (count-number (rest los)))]))

;(less-or-same los-2 3)
;(count-number los-2)


;(define (all-greater? los n)
;  (cond [ (= (count-number (less-or-same los n)) 0) #true]
;        [ else #false]))

;(all-greater? los-2 3) 

;Exercise 3

;LON is one of
;empty
;(cons number LON)

(define lon-1 (cons 1 (cons 2 (cons 4 empty))))

;lon n -> lon

(define (add-number-to lon n)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (+ n (first lon)) (add-number-to (rest lon) n))]))

;(add-number-to lon-1 3)


 
;Exercise 4

; lon-> lon
(define lon-2 (cons 0 (cons 2 (cons 4 empty))))

(define (divide lon n)
  (cond [(empty? lon) lon]
        [(cons? lon) (cond [(= 0 (first lon)) (cons "DIVO" (divide (rest lon) n))]
                           [else (cons (/(first lon)n) (divide (rest lon)n))])]))

(divide lon-2 0.5)


;Exercise 5

;LoS is one of
;empty
;(cons string LoS)

(define list1 (cons "READ" (cons "BIRD" empty)))

(text "READ" 10 "red")
(text "BIRD" 10 "blue")

;get next list
;los->los
(define (get-next los)
  (cond [(empty? los) los]
        [(cons? los) (rest los)]))

;draw first list
; list-> pic
(define (draw-first los)
  (cond [(empty? los) (empty-scene 300 300)]
        [else (place-image (text (first los) 50 "red")
                                  150
                                  150
                                  (empty-scene 300 300))]))
 
;click-mouse
;los x y string -> los
(define (click-mouse los x y string)
  (cond [(string=? string "button-down") (get-next los)]
        [else los]))

(define (lets-go los)
 (big-bang los
          (to-draw draw-first)
          (on-mouse click-mouse)))

(lets-go list1)








                     