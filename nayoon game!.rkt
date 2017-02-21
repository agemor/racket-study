;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |nayoon game!|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;; CONSTANTS ;;;;
(define GRID-HEIGHT 41) ;# cells in grid
(define GRID-WIDTH 41)
(define CELL-HEIGHT 15) ;cells are 15px X 15px
(define CELL-WIDTH 15)
(define ACTIVE-COLOR "green")
(define TYPING-COLOR "purple")
(define STUCK-COLOR "red")
(define SCENE-HEIGHT (* GRID-HEIGHT CELL-HEIGHT))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH)) 
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;;DATA DEFINITIONS;;

;A WorldState (WS) is
(define-struct world [low key-input tick])

(define WS1 (make-world empty "" 0))
(define WS2 (make-world empty "WORDS" 0))


                            ;key-input score])

;(1)A List of Words (LOW) 
;(2)Low
;(3)tick value

;A Word is a (make-word String Color Posn Boolean)
(define-struct word [vocab color position stuck?])

(define LOW0 empty)
(define WORD1 (make-word "dog" ACTIVE-COLOR (make-posn 5 5) #false))
(define WORD2 (make-word "cat" ACTIVE-COLOR (make-posn 6 7) #true))
(define WORD3 (make-word "fish" ACTIVE-COLOR (make-posn 8 4) #false))
  
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



;LOL->Image
;takes in a list of letters and outputs an image

(define (draw-lol lol ws)
  (cond [(empty? lol) (place-image (text (world-key-input ws) 15 TYPING-COLOR) 300 600 SCENE)]
        [else (place-image (draw-letter (first lol))
                           (posnx->cell (letter-position (first lol)))
                           (posny->cell (letter-position (first lol)))
                           (draw-lol (rest lol) ws ))]))
 
;LOW->Image
;takes in a list of word and outputs an image

(define (draw-low low ws)
  (draw-lol (LOW->LOL low) ws))



;SW->Image
(define (draw-ws sw)
  (draw-low (world-low sw) sw))
           
;;;;;;on-tick function;;;;


;number-words: low-> number 
(define (number-words low)
  (cond [(empty? low) 0]
        [else (+ 1 (number-words (rest low)))]))
 
 
;LOW -> LOW
(define (update-word-position low)
  (cond [(empty? low) empty]
   [else  (cond [(check-collision? (rest low) (first low))
            (cons (make-word (word-vocab (first low)) STUCK-COLOR (word-position (first low)) #true)
                  (update-word-position (rest low)))]
          [else (cons (make-word (word-vocab (first low))
                           (word-color (first low))
                           (make-posn (posn-x (word-position (first low))) (+ 1 (posn-y (word-position (first low)))))
                           (word-stuck? (first low)))
                           (update-word-position (rest low)))])])) 

;SW->SW 

(define (update-WORLD world)
  (make-world (update-low world) (world-key-input  world) (+ 1 (world-tick world))))

  

(define (check-collision? low word) 
    (cond [(empty? low) (= (posn-y (word-position word)) 39)]
                            
          [else (cond [ (and (= (+ (posn-y (word-position word)) 1) (posn-y (word-position (first low))))
                             (> (posn-x (word-position word))  ( - (posn-x (word-position (first low))) (string-length (word-vocab word))))
                             (> (+ (posn-x (word-position (first low))) (string-length (word-vocab (first low)))) (posn-x (word-position word)))) #true]
                      [else (check-collision? (rest low) word)])]))

(define LOV (list "floor" "sky" "coffee" "computer" "ground" "chalk" "marker" "pigeon" "dog" "cat" "bird" "turtle" "parrot"
                  "printer" "whale" "dolphin" "fish" "porpise" "seagull" "ferret" "shark" "jellyfish" "peach" "plum" "pear" "grape"))

;LOW -> LOW
;Adds a new random word to given low      !! CHECK EXPECT RANODM
(define (add-random-word low)
     (cons (get-random (list-ref LOV (random (length LOV)))) low))
            
      ;(get-random (random (length LOV))) '())]
    ;[else (cons (first low) (add-random-word (rest low)))]))



;String -> Word
;Returns new Word to be placed on screen      !! CHECK EXPECT RANODM
(define (get-random phrase)
  (make-word phrase
             ACTIVE-COLOR
             (make-posn (get-random-x phrase) 0)
             #false))
 
;SW->LOW
(define (get-random-x phrase)
  (add1 (random (- 40 1 (string-length phrase)))))

;(define (generate-new-word sw)
;   (cond [(even? (world-tick sw)) (cons (make-word (Name LOV1) ACTIVE-COLOR (make-posn (get-random-x (Name LOV1) 0) #false) (world-low sw))]
;       [else (world-low sw)])

;SW->LOW
(define (add-word sw)
 (cond [(even? (world-tick sw)) (add-random-word (world-low sw))]
       [else (world-low sw)]))

(add-word WS2)
 
;(define (Name LOV) (list-ref LOV (random (length LOV))))

;(define LOV1 (list "dog" "fish" "cat" "rabbit"))

;sw->low
(define (update-low sw)
    (update-word-position (add-word sw)));;

;;;;stop-when;;;
 
(define (Game-over?? low )
  (cond [(empty? low) #false]
  [else (cond [(and (check-collision? (rest low) (first low)) (= (posn-y (word-position (first low))) 0)) #true]
              [else #false])]))


(define (game-over? sw)
  (Game-over?? (world-low sw)))

;;;on-key;;;


(define (place-string s)
  (place-image (text s 15 TYPING-COLOR) 300 600 (empty-scene 600 630)))


(define (handler sw string)
  (cond [(string=? string "\b") (delete sw) ]
        [(string=? string "\r") (enter sw)]
        [(string-alphabetic? string) (write sw string)]
        [else sw]))


;(define-struct world [low key-input tick])

(define (write ws string)
  (make-world (world-low ws)
              (string-append (world-key-input ws) string)
              (world-tick ws)))
   

  (define (delete ws)
    (make-world (world-low ws)
                (substring (world-key-input ws) 0 ( - (string-length (world-key-input ws) ) 1))
                (world-tick ws)))

  (delete WS2)
  
 
  (define (enter ws)
    (make-world
     (remove-word-state (world-low ws) (what-word (string-list (world-key-input ws) (active-list (world-low ws)))))
     ""
     (world-tick ws)))

   (define (active-list low)
     (cond [(empty? low) empty]
           [else (cond [ (word-stuck? (first low)) (active-list (rest low))]
                       [ else (cons (first low) (active-list (rest low)))])]))
   
;string-list: String -> LOW
;returns a list of all instanced of the string
   (define (string-list string low)
     (cond [(empty? low) empty]
           [else (if (string=? (word-vocab (first low)) string)
                       (cons (first low) (string-list string (rest low)))
                       (string-list string (rest low)))]))
;LOW -> Word
   (define (what-word low)
     (cond [(empty? low) ""] 
      [else (first (reverse low))]))

(what-word LOW2)
   
;low Word -> LOW
   (define (remove-word-state low word)
     (cond [(empty? low) empty]
           [else (if (equal? word (first low))
                     (remove-word-state (rest low) word)
                     (cons (first low) (remove-word-state (rest low) word)))]))

   (remove-word-state LOW2 WORD2)
   
  
 

;(define (blah sw string)
;  (cond
;    [;if input is enter, do x
;     [; if input is delete, do y
;      [;else, IF string-alpahbetic, add letter
(define (main num)
  (* num (world-tick
          (big-bang WS1
    (to-draw draw-ws)      
    (on-tick update-WORLD num)
    (on-key handler)
    (stop-when game-over?)))))
 
