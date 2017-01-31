;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-3.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;A ballon state (bs) is 

(define-struct balloon-state [radius])
(define BA (make-balloon-state 20))

;num is the circle's radius in pixels

(define balloon1 100)

;constants
(define BACKGROUND (empty-scene 500 500))
(define RADIUS-START 150)
(define PLACE-START (make-posn 250 250))
(define COLOR-START "RED")
(define CIRCLE-START (circle 150 "solid" "red"))
 
;ball length
;state-> state

(define (lengthX bs)
  ( make-balloon-state (+ (balloon-state-radius bs) 10)))

(check-expect (lengthX BA) (make-balloon-state 30))

;picture
;state-> image

(define (picture bs)
  (place-image (circle (balloon-state-radius bs) "solid" "red") 250 250 BACKGROUND))

(check-expect (picture BA) (place-image (circle 20 "solid" "red") 250 250 BACKGROUND))
 
;click
;bs x y string -> bs

(define (click bs x y string)
  (cond [(string=? string "button down") (make-balloon-state (- (balloon-state-radius bs) 10))]
        [else bs]))

(check-expect (click BA 10 10 "button down") (make-balloon-state 10))
(check-expect (click BA 10 10 "button ") (make-balloon-state 20))

;bigbang  

(big-bang CIRCLE-START
          [on-tick lengthX 0.1]
          [to-draw picture]
          [on-mouse click])