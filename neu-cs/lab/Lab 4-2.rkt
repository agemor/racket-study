;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 4-2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;#7
;; A Ball is (make-ball posn posn)
(define-struct ball [position velocity])
(define-struct vector [x y])


(define BALL1 (make-ball (make-vector 1 2) (make-vector 1 1)))
(define SHAPE1 (circle 30 "solid" "red"))

;#8
;A LOB (list of balls) is one of:
;-empty
;-(cons ball LOB)
(define LOB-1
  (cons BALL1 (cons (make-ball (make-vector 20 40) (make-vector 4 5)) empty)))


;#
;(define (ball-tmpl ball)
;  ...(make-vector (vector-x (ball-position ball)) (vector-y (ball-position ball)))
;  ...(make-vector (vector-x (ball-velocity ball)) (vector-y (ball-velocity ball)))...)

;#
;(define (lob-tmpl lob)
;   (cond [(empty? lob) ...]
;              [(cons? lob) (
;                     ...  (first lob) ...
;                     ...  (lob-tmpl (rest lob)) ...
;                            )]))

;#10
(define (draw-ball ball image)
  (place-image SHAPE1
              (vector-x (ball-position ball))
              (vector-y (ball-position ball))
              image))

(draw-ball BALL1 (empty-scene 500 500))

 ;#11
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 500)
(define BALL-RADIUS 10)

;lob-> image
(define (draw-lob lob)
    (cond [(empty? lob) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)]
          [else  (draw-ball (first lob) (draw-lob (rest lob)))]))



 ;#12
;ball->boolean
  (define (off-screen? ball)
    (cond [(and (<= (vector-x (ball-position ball)) (+ BALL-RADIUS CANVAS-WIDTH))
           (>= (vector-x (ball-position ball)) -10)) #false]
               [(and (<= (vector-y (ball-position ball)) (+ BALL-RADIUS CANVAS-HEIGHT))
           (>= (vector-y (ball-position ball)) -10)) #false]
               [else #true]))

 ;#13
 ;LOB->LOB
(define (on-screen lob)
  (cond [(empty? lob) lob]
            [(cons? lob) (cond [(off-screen? (first lob)) (on-screen (rest lob))]
                               [else (cons (first lob) (on-screen (rest lob)))])]))

 ;#14
 ;ball-> ball
(define (gravity ball)
  (make-ball (make-vector (vector-x (ball-position ball))
             (vector-y (ball-position ball)))
             (make-vector (vector-x (ball-velocity ball))
             (+  9.8 (vector-y (ball-velocity ball))))))

 

;#15
;LoB-> LoB
(define (apply-gravity lob)
  (cond [(empty? lob) lob]
        [(cons? lob) (cons (gravity (first lob)) (apply-gravity (rest lob)))]))


;#16
;ball->ball
(define (move-ball ball)
  (make-ball (make-vector (+ (vector-x (ball-position ball))
                             (vector-x (ball-velocity ball)))
                          (+ (vector-y (ball-position ball))
                             (vector-y (ball-velocity ball))))
             (make-vector (vector-x (ball-velocity ball))
             (vector-y (ball-velocity ball)))))



;#17
;LOB->LOB

(define (move-all lob)
  (cond [(empty? lob) lob]
        [(cons? lob) (cons (move-ball (first lob)) (move-all (rest lob)))]))


;#18
;LOB-> LOB
(define (new-ball lob x y string)
  (cond
    [(string=? string "button-down")
     (cons (make-ball (make-vector x y) (make-vector (random 100) (random 100)))
           (cons (first lob) (rest lob)))]
    [else lob]))

; go : LoB -> LoB
; Move balls, apply gravity, and then filter out those balls that are off screen
(define (go lob)
  (on-screen (apply-gravity (move-all lob))))

(go LOB-1)


; main : Ball -> LoB
 (define (main b)
  (big-bang (cons b empty) ; <- the world state is a LoB
            [to-draw draw-lob]
            [on-tick go]
            [on-mouse new-ball]))
 
 

(main BALL1)                                                                    
  
  



















  