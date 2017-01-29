;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 2 Little game|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A LocationWorld (LW) is a (make-posn Number Number)
; and represents where our dot is on the screen in pixel coordinates
; (define-struct posn [x y]) <- note, we don't need this! posns come with BSL


(define LW-1 (make-posn 20 20))


(define DOT (circle 10 "solid" "black" ))

(define (draw-location-world LW)
  (place-image DOT (posn-x LW) (posn-y LW) (empty-scene 300 300)))

; LW Number -> LW
(define (move-up LW number)
  (make-posn (posn-x LW) (- (posn-y LW) number)))

(check-expect (move-up LW-1 10) (make-posn 20 10))

; LW String -> LW

(define (move LW string)
  (cond [(string=? string "up")
        (make-posn (posn-x LW) (- (posn-y LW)10))]
        [(string=? string "right")
         (make-posn (+ (posn-x LW) 10) (posn-y LW))]
        [(string=? string "down")
         (make-posn (posn-x LW) (+ (posn-y LW) 10))]
        [(string=? string "left")
         (make-posn (- (posn-x LW) 10) (posn-y LW))]))
 
(big-bang LW-1
          [to-draw draw-location-world]
          [on-key move])
        

; (big-bang BALL
         ;   (check-with image?)
          ;  (on-draw draw-location-world)
           ; (on-mouse draw-location-world))
;(big-bang ballA
       ;   [to-draw draw-location-world]
        ;  [on-key draw-location-world])
        


