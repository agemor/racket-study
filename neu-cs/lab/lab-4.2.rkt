#lang racket


;;
;;   VELOCITY 0 -> 9.8 -> 9.8*2 -> 9.8*3
;;   POSITION 0 -> 9.8 -> 9.8 + 9.8*2 -> 9.8 + 9.8*2  + 9.8*3 
;;   Y
;;   Y

(require 2htdp/image)
(require 2htdp/universe)
 
; main : Ball -> LoB
(define (main b)
  (big-bang (cons b '()) ; <- the world state is a LoB
            [to-draw draw-lob]
            [on-tick go]
            [on-mouse new-ball]))
 
; go : LoB -> LoB
; Move balls, apply gravity, and then filter out those balls that are off screen
(define (go lob)
  (on-screen-balls (apply-gravity (move-all lob))))

;#7
;; A Ball is (make-ball posn posn)
(define-struct ball [position velocity])
(define-struct vector [x y])

(define BALL1 (make-ball (make-vector 1 2) (make-vector 3 4)))
(define SHAPE1 (circle 10 "solid" "red"))
;#8
;A LOB (list of balls) is one of:
;-empty
;-(cons ball LOB)
(define LOB-1 (con (make-ball (make-vector 1 3) (make-vector 4 5)) empty))

; (cons left right)
; (cons left (cons left1 right1))
; (cons (cons left1 right1) right)

;#
(cons 'nayoon 'hyunjun)

;#
(define (ball-tmpl ball)
  ...(vector-x (ball-position ball))...
  ...(vector-y (ball-position ball))...
  ...(vector-x (ball-velocity ball))...
  ...(vector-y (ball-velocity ball))...)

;#
(define (lob-tmpl lob)
   (cond [(empty? lob) ...]
              [(cons? lob) (
                     ...  (first lob) ...
                     ...  (lob-tmpl (rest lob)) ...
                            )])

;#10
(define (draw-ball ball image)
  (place-image (circle 10 "solid" "red")
              (vector-x (ball-position ball))
              (vector-y (ball-position ball))
              image))

 ;#11
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 500)
(define BALL-RADIUS 10)
(define (draw-lob lob)
    (cond [(empty? lob) (empty-screen CANVAS-WIDTH CANVAS-HEIGHT)]
               [(cons? lob)  (draw-ball (first lob)  (draw-lob (rest lob)))]))

 ;#12
;ball->boolean
  (define (off-screen? ball)
    (cond [(and (<= (vector-x (ball-position ball)) (+ BALL-RADIUS CANVAS-WIDTH))
           (>= (vector-x (ball-position ball) -10))) #false]
               [(and (<= (vector-y (ball-position ball)) (+ BALL-RADIUS CANVAS-HEIGHT))
           (>= (vector-y (ball-position ball) -10))) #false]
               [else #true]))

 ;#13
  ;LOB->LOB
(define (on-screen lob)
  (cond [(empty? lob) lob]
            [(cons? lob) (cond [(off-screen? (first lob)) (on-screen (rest lob))]
                                            [else (cons (first lob) (on-screen (rest lob)))])]))

 ;#14

  
  
                                 
                                            
                                
  

  
    
  
  

  



