;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-3.3-hyunjun) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

(require 2htdp/image)
(require 2htdp/universe)

; balloon-state
(define-struct balloon-state [shrink? radius time])

;constants
(define BACKGROUND (empty-scene 500 500))
(define BALLOON-STATE (make-balloon-state #false 10 0))
(define BALLOON-SHRINK-THRESHOLD 5)

(define SHRINK-TIME0 10)
(define SHRINK-TIME1 40)
(define SHRINK-TIME2 60)

;; update
(define (update bs)
    (make-balloon-state
        (balloon-state-shrink? bs)
        (get-next-radius bs)
        (+ 1 (balloon-state-time bs)))
  )

; get-radius-delta
(define (get-radius-delta bs)
    (cond [(balloon-state-shrink? bs)
                  (cond [(< (balloon-state-time bs) SHRINK-TIME0) 0.1]
                             [(< (balloon-state-time bs) SHRINK-TIME1) 0.3]
                             [(< (balloon-state-time bs) SHRINK-TIME2) 0.7]
                             [else 1])]
               [else 1])
  )

; get-next-radius
(define (get-next-radius bs)
    (cond [(balloon-state-shrink? bs)
                 (- (balloon-state-radius bs) (get-radius-delta bs))]
             [else
                 (+ (balloon-state-radius bs) (get-radius-delta bs))]))

(define (at-goal? bs)
    (< (balloon-state-radius bs) BALLOON-SHRINK-THRESHOLD))

; render
(define (render bs)
    (place-image (circle (balloon-state-radius bs) "solid" "blue") 250 250 BACKGROUND))

; mouse-event-handler
(define (mouse-event-handler bs x y mouse-event)
    (cond [(string=? mouse-event "button-down")
         (make-balloon-state
             #true
             (balloon-state-radius bs)
             (+ 1 (balloon-state-time bs)))]
         [else bs]))

;play-balloon  
(define (play-balloon tick-rate)
    (big-bang BALLOON-STATE
          [on-tick update tick-rate]
          [to-draw render]
          [on-mouse mouse-event-handler]
          [stop-when at-goal?]))

(play-balloon 0.01)





