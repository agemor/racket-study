;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |nayoon game!|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;; CONSTANTS ;;;;
(define GRID-HEIGHT 40) ;# cells in grid
(define GRID-WIDTH 40)
(define CELL-HEIGHT 15) ;cells are 15px X 15px
(define CELL-WIDTH 15)
(define ACTIVE-COLOR "green")
(define TYPING-COLOR "purple")
(define STUCK-COLOR "red")
(define SCENE-HEIGHT (* GRID-HEIGHT CELL-HEIGHT))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;;DATA DEFINITIONS;;

;A WorldState (WS) is 3

;(1)A List of Words (LOW) 
;(2)Low
;(3)tick value

;A Word is a (make-word String Color Posn Boolean)
(define-struct word [vocab color position stuck?])

(define WORD1 (make-word "dog" ACTIVE-COLOR (make-posn 5 5) #false))
(define WORD2 (make-word "cat" TYPING-COLOR (make-posn 6 7) #true))
(define WORD3 (make-word "fish" STUCK-COLOR (make-posn 8 4) #false))
  
;A Letter is a (make-letter String Posn String)
(define-struct letter [character position color ])

(define LETTER1 (make-letter "d" (make-posn 5 5) ACTIVE-COLOR))
(define LETTER2 (make-letter "o" (make-posn 6 5) ACTIVE-COLOR))
(define LETTER3 (make-letter "g" (make-posn 7 5) ACTIVE-COLOR))
  
;LOW (List of Word)
; – '()
; – (cons Word LOW)
(define LOW1 '())
(define LOW2 (cons WORD1 (cons WORD2 (cons WORD3 '()))))
(define LOW3 (cons WORD1 '()))
(define LOW4 (cons WORD1 (cons WORD2 '())))
  
;LOL (List of Letter)
; – '()
; – (cons Letteru LOL)
(define LOL1 '())
(define LOL2 (cons LETTER1 (cons LETTER2 (cons LETTER3 '()))))
  
;A Color is one of:
; – ACTIVE-COLOR
; – TYPING-COLOR
; – STUCK-COLOR

;;;make a letter;;;

;;;Posn-> Cell;;;

(define (posnx->cell posn)
   (* (posn-x posn) 15))
  
(define (posny->cell posn)
 (* (posn-y posn) 15))
  
(posnx->cell (make-posn 5 5))

(posny->cell (make-posn 5 5))

;WORD -> LOS
;takes a word and outputs a list of strings
(define (split w)
  (explode (word-vocab w)))
  
(check-expect (split WORD1) (cons "d" (cons "o" (cons "g" empty))))
(define LOS1 (cons "d" (cons "o" (cons "g" empty))))

;LOS Posn Color -> LOL
;takes in a list of strings, posn, and color and outputs a list of letters

(define (create-lol los posn color)
  (cond [(empty? los) empty]
        [else (cons (make-letter (first los)
                                   (make-posn (posn-x posn)
                                              (posn-y posn)) color)
                    (create-lol (rest los)
                                (make-posn (+ (posn-x posn) 1)
                                             (posn-y posn)) color))]))

(create-lol LOS1 (make-posn 5 5) ACTIVE-COLOR)

;Letter -> Image
;takes in a letter and outputs an image

(define (draw-letter letter)
  (text (letter-character letter) 15 (letter-color letter)))

(draw-letter LETTER1)

;LOW -> LOL
;takes in a list of words and outputs a list of letters
(define (LOW->LOL low)
  (cond [(empty? low) empty]
        [(cons? low)
         (append
          (create-lol (split (first low)) (word-position (first low)) (word-color (first low)))
          (LOW->LOL (rest low)))]))

(LOW->LOL LOW2)

;LOL->Image
;takes in a list of letters and outputs an image

(define (draw-lol lol)
  (cond [(empty? lol) SCENE]
        [else (place-image (draw-letter (first lol))
                           (posnx->cell (letter-position (first lol)))
                           (posny->cell (letter-position (first lol)))
                           (draw-lol (rest lol) ))]))

;LOW->Image
;takes in a list of word and outputs an image

(define (draw-low low)
  (draw-lol (LOW->LOL low)))

(draw-low LOW2)
           



                               
  



  
