;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))



; == Data Definition
; A FanFic is a (make-fanfic String String String String Nat)
(define-struct fanfic [title author fandom body likes])

; == Data Interpretation
; - where title is the title of the fanfic
; - author is the username of the author
; - fandom is the name of the fandom to which the fanfic belongs
; - body is the contents of the fanfic
; - likes is the number of likes it has received

;== Data Examples
(define fanficC (make-fanfic "Best Days" "Nick Williams" "Sad Days" "He was mad at me" 10))
(define fanficD (make-fanfic "Good Dogs" "Nate Willis" "Sad Cats" "He was looking at me" 0))

;== Data Template
#;
(define (fanfic-tmpl X)
  ... (fanfic-title X) ...
  ... (fanfic-author X) ...
  ... (fanfic-fandom X)...
  ... (fanfic-body X)...
  ... (fanfic-likes X)...)

;Exercise 1

; == Function Signature
;Fanfic-> Fanfic

; == Function Purpose statement
; Make a function that has 1 more likes than the given FanFic

; == Code
(define (like fanficX)
  (make-fanfic
   (fanfic-title fanficX)
   (fanfic-author fanficX)
   (fanfic-fandom fanficX)
   (fanfic-body fanficX)
   (+ (fanfic-likes fanficX) 1)))

; == Function Tests (check-expect)
 (check-expect (like fanficC)
               (make-fanfic "Best Days" "Nick Williams" "Sad Days" "He was mad at me" 11))

 (check-expect (like fanficD)
               (make-fanfic "Good Dogs" "Nate Willis" "Sad Cats" "He was looking at me" 1))

;Exercise 2

; == Function Signature
;Fanfic-> Fanfic

; == Function Purpose statement
; Make a function that has 1 less likes than the given FanFic

;== Data Examples
; same examples as above fanficC and fanficD


; == Code
(define (dislike fanficX)
  (make-fanfic
   (fanfic-title fanficX)
   (fanfic-author fanficX)
   (fanfic-fandom fanficX)
   (fanfic-body fanficX)
   (cond [(< 0 (fanfic-likes fanficX)) (- (fanfic-likes fanficX) 1)]
         [else 0]))) 
 
; == Function Tests (check-expect)
(check-expect (dislike fanficC)
               (make-fanfic "Best Days" "Nick Williams" "Sad Days" "He was mad at me" 9))

(check-expect (dislike fanficD)
              (make-fanfic "Good Dogs" "Nate Willis" "Sad Cats" "He was looking at me" 0))
             
;Exercise 3

; == Function Signature
;Fanfic string -> Fanfic

; == Function Purpose statement
; Make a function that adds the given string to the body of the given fanfic

;== Data Examples
; same examples as above fanficC and fanficD

; == Code
(define (add-text fanficX string)
  (make-fanfic
   (fanfic-title fanficX)
   (fanfic-author fanficX)
   (fanfic-fandom fanficX)
   (string-append (fanfic-body fanficX) string)
   (fanfic-likes fanficX)))
 
; == Function Tests (check-expect) 
 (check-expect (add-text fanficC " yeah?")
               (make-fanfic "Best Days" "Nick Williams" "Sad Days" "He was mad at me yeah?" 10))

(check-expect (add-text fanficD " Oh!")
              (make-fanfic "Good Dogs" "Nate Willis" "Sad Cats" "He was looking at me Oh!" 0))

;Exercise 4


;Data Definition
;A World-state is a posn

;Date Interpretation
;A World-state indicates the location of the ball

;Date Examples
(define start-position (make-posn 150 150)) ;initial world state

(require 2htdp/image)
(require 2htdp/universe)

(define BALL (circle 10 "solid" "black" )) ;

;Function Signature
;posn->image

;Function Purpose Statement
;Make a function that draws the ball at posn-x posn-y of the given posn on the empty scene.

(define (draw-the-ball ponsX)
  (place-image BALL (posn-x ponsX) (posn-y ponsX) (empty-scene 300 300)))

(check-expect (draw-the-ball (make-posn 10 10 ))
              (place-image BALL 10 10 (empty-scene 300 300)))

;Function Signature
;posn number number string -> posn

;Function Purpose Statement
;if mouse has been clicked, returns Posn of click coordinates; else returns same Posn

(define (Update-position posnX x y string)
  (cond
    [(string=? string "button-down") (make-posn x y)]
    [else posnX]))

(check-expect (Update-position (make-posn 10 10 ) 20 20 "button-down")
              (make-posn 20 20 ))
      
(check-expect (Update-position (make-posn 10 10 ) 20 20 "happy")
              (make-posn 10 10 ))
      
 
(big-bang start-position
           (to-draw draw-the-ball)
            (on-mouse Update-position))









