;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname circle) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

(require 2htdp/image)
(require 2htdp/universe)

;A ballon state is a [Num]

(define-struct balloon-state [radius])
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

(define (length1 bs)
  ( + bs 10))

(define (length2 bs)
  (- bs 10))
 
;picture
;state-> image

(define (picture bs)
  (circle (length1 bs) "solid" "red") )

;click
;bs x y string -> bs

(define (click bs x y string)
  (cond [(string=? string "button down") (length2 bs)]
        [else bs]))

;bigbang  

(big-bang CIRCLE-START
          [on-tick length1 0.1]
          [to-draw picture]
          [on-mouse click])





