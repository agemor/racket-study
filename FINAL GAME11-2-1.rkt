;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |FINAL GAME11-2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 10
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

;A WorldState (WS) is (make-world LOW String Number)
(define-struct world [low key-input tick])
; – world?
; – world-low
; –– where low is the list of all words currently on the screen
; – world-key-input
; –– where key-input is what the string user is currently typing
; – world-tick
; – make-world

(define WS1 (make-world empty "" 0))
(define WS2 (make-world empty "WORDS" 0))


;WS -> ????
#;(define (ws-tmplt ws)
  ... (world-low ws)...
  ... (word-key-input ws)...
  ... (world-tick ws)...)

;A Word is a (make-word String Color Posn Boolean)
(define-struct word [vocab color position stuck?])
; – word?
; – word-vocab
; – word-color
; – word-position
; –– where the Posn represents the left-most letter in word, in grid coordinates [0 40]
; – word-stuck?
; – make-word

(define WORD1 (make-word "dog" ACTIVE-COLOR (make-posn 5 5) #false))
(define WORD2 (make-word "cat" ACTIVE-COLOR (make-posn 6 7) #true))
(define WORD3 (make-word "fish" ACTIVE-COLOR (make-posn 8 4) #false))
(define WORD4 (make-word "fish" ACTIVE-COLOR (make-posn 10 3) #false))
(define WORD5 (make-word "fish" ACTIVE-COLOR (make-posn 8 4) #true))
(define WORD6 (make-word "yellow" ACTIVE-COLOR (make-posn 8 39) #true))
(define WORD7 (make-word "good" ACTIVE-COLOR (make-posn 8 0) #true))
(define WORD8 (make-word "yellow" ACTIVE-COLOR (make-posn 9 1) #true))

;Word -> ????
#;(define (word-tmplt word)
  ... (word-vocab word)...
  ... (word-color word)...
  ... (posn-x (word-position word))...
  ... (posn-y (word-position word))...
  ... (word-stuck? word)...)

;A LOW (List of Words) is one of
; – '()
; – (cons Word LOW)

;LOW -> ????
#;(define (low-tmplt low)
  (cond
    [(empty? low)...]
    [else ... (word-tmplt (first low))...
          ... (low-tmplt (rest low)...)...]))

(define LOW1 '())
(define LOW2 (cons WORD1 (cons WORD2 (cons WORD3 '()))))
(define LOW3 (cons WORD1 '()))
(define LOW4 (cons WORD1 (cons WORD2 '())))
(define LOW5 (cons WORD5 (cons WORD1 (cons WORD3 '()))))
(define LOW6 (cons WORD4 (cons WORD5 (cons WORD3 '()))))
(define LOW7 (cons WORD7 (cons WORD8 (cons WORD3 '()))))
(define WS3 (make-world LOW2 "dog" 0))
(define WS4 (make-world LOW7 "dog" 0))
(define WS5 (make-world LOW2 "dog" 1))
(define WS6 (make-world LOW2 "dog" 2))

;A Letter is a (make-letter String Posn String)
(define-struct letter [character position color])
; – letter?
; – letter-character
; – letter-position
; –– where the Posn represents the grid coordinates of Letter [0 40]
; – letter-color
; – make-letter

(define LETTER1 (make-letter "d" (make-posn 5 5) ACTIVE-COLOR))
(define LETTER2 (make-letter "o" (make-posn 6 5) ACTIVE-COLOR))
(define LETTER3 (make-letter "g" (make-posn 7 5) ACTIVE-COLOR))

;Letter -> ????
#;(define (letter-tmplt letter)
  ... (letter-character letter)...
  ... (posn-x (letter-position letter))...
  ... (posn-y (letter-position letter))...
  ... (letter-color letter)...)  

;LOL (List of Letter)
; – '()
; – (cons Letteru LOL)

(define LOL1 '())
(define LOL2 (cons LETTER1 (cons LETTER2 (cons LETTER3 '()))))

;LOL -> ????
#;(define (lol-tmplt lol)
  (cond
    [(empty? lol)...]
    [else ... (letter-tmplt (first lol))...
          ... (lol-tmplt (rest lol))]))

;Where LOV is the list of possible (word-vocab) values
(define LOV (list "floor" "sky" "coffee" "computer" "ground" "chalk" "marker"
                  "pigeon" "dog" "cat" "bird" "turtle" "parrot"
                  "printer" "whale" "dolphin" "fish" "porpise" "seagull" "ferret" "shark"
                  "jellyfish" "peach" "plum" "pear" "grape" "fish"))

;A Color is one of:
; – ACTIVE-COLOR
; – TYPING-COLOR
; – STUCK-COLOR

;A List of Strings (LOS) is one of:
; – '()
; – (cons String LOS)

(define LOS0 empty)
(define LOS1 (cons "d" (cons "o" (cons "g" empty))))

;LOS -> ????
#;(define (los-tmplt los)
  (cond
    [(empty? los)...]
    [else... (first los)... (los-tmplt (rest low))]))


;;;;;;;;;;;;;;;Posn->Cell;;;;;;;;;;;;;

;Posn -> Number
;Returns (posn-x) in cell-coordinate in equivalent pixels (* 15)
(define (posnx->cell posn)
   (* (posn-x posn) 15))

(check-expect (posnx->cell (make-posn 5 5)) 75)
(check-expect (posnx->cell (make-posn 6 5)) 90)

;Posn -> Number
;Returns (posn-y) in cell-coordinate in equivalent pixels (* 15)
(define (posny->cell posn)
 (* (posn-y posn) 15))

(check-expect (posny->cell (make-posn 5 5)) 75)
(check-expect (posny->cell (make-posn 5 6)) 90)


;;;;;;;;;;;;;;;;;;;;;;LOS;;;;;;;;;;;;;;;;;;;;;;

;Word -> LOS
;Returns individual letters in (word-vocab) as a list of 1Strings
(define (split w)
  (explode (word-vocab w)))
  
(check-expect (split WORD1) (cons "d" (cons "o" (cons "g" empty))))

;LOS Posn Color -> LOL
;Retruns a LOL with all values in LOS as letters with coordinates beside eachother (x)
(define (create-lol los posn color)
  (cond [(empty? los) empty]
        [else (cons (make-letter (first los)
                                 (make-posn (posn-x posn)
                                            (posn-y posn)) color)
                    (create-lol (rest los)
                                (make-posn (+ (posn-x posn) 1)
                                             (posn-y posn)) color))]))

(check-expect (create-lol LOS0 (make-posn 1 1) ACTIVE-COLOR) empty)
(check-expect (create-lol LOS1 (make-posn 5 5) ACTIVE-COLOR)
              (cons (make-letter "d" (make-posn 5 5) "green")
              (cons (make-letter "o" (make-posn 6 5) "green")
              (cons (make-letter "g" (make-posn 7 5) "green") '()))))

;Letter -> Image
;Returns an image of given letter in (text) format with regard to (letter-color)
(define (draw-letter letter)
  (text (letter-character letter) 15 (letter-color letter)))

(check-expect (draw-letter LETTER1) (text "d" 15 ACTIVE-COLOR))

;LOW -> LOL
;Retruns a list of letters of all values in LOW
(define (LOW->LOL low)
  (cond [(empty? low) empty]
        [(cons? low) 
         (append
          (create-lol (split (first low)) (word-position (first low)) (word-color (first low)))
          (LOW->LOL (rest low)))]))

(check-expect (LOW->LOL LOW1) empty)
(check-expect (LOW->LOL LOW2)
              (cons (make-letter "d" (make-posn 5 5) "green")
              (cons (make-letter "o" (make-posn 6 5) "green")
              (cons (make-letter "g" (make-posn 7 5) "green")
              (cons (make-letter "c" (make-posn 6 7) "green")
              (cons (make-letter "a" (make-posn 7 7) "green")
              (cons (make-letter "t" (make-posn 8 7) "green")
              (cons (make-letter "f" (make-posn 8 4) "green")
              (cons (make-letter "i" (make-posn 9 4) "green")
              (cons (make-letter "s" (make-posn 10 4) "green")
              (cons (make-letter "h" (make-posn 11 4) "green") '())))))))))))

;LOL WS -> Image
;Places all letters in LOL onto SCENE
(define (draw-lol lol ws)
  (cond [(empty? lol) SCENE]
        [else (place-image (draw-letter (first lol))
                           (posnx->cell (letter-position (first lol)))
                           (posny->cell (letter-position (first lol)))
                           (draw-lol (rest lol) ws ))]))

(check-expect (draw-lol LOL1 WS1) SCENE)
(check-expect (draw-lol LOL2 WS1) (place-image (text "d" 15 ACTIVE-COLOR) 75 75
                                  (place-image (text "o" 15 ACTIVE-COLOR) 90 75
                                  (place-image (text "g" 15 ACTIVE-COLOR) 105 75
                                  (place-image (text "" 25 TYPING-COLOR) 300 600 SCENE)))))
 
;LOW WS -> Image
;Places all words in LOW onto SCENE
(define (draw-low low ws)
  (draw-lol (LOW->LOL low) ws))

(check-expect (draw-low LOW3 WS1)
              (place-image (text "d" 15 ACTIVE-COLOR) 75 75
                                  (place-image (text "o" 15 ACTIVE-COLOR) 90 75
                                  (place-image (text "g" 15 ACTIVE-COLOR) 105 75
                                  (place-image (text "" 25 TYPING-COLOR) 300 600 SCENE)))))
(check-expect (draw-low '() WS1)
              (place-image (text "" 25 TYPING-COLOR) 300 600 SCENE))


;WS -> Image
;Places all words in (world-low) on screen above (world-key-input)
(define (draw-ws ws)
  (above
  (draw-low (world-low ws) ws)
  (text (world-key-input ws) 15 TYPING-COLOR)))

(check-expect (draw-ws WS1)
              (above
              SCENE
              (text "" 15 TYPING-COLOR)))

(check-expect (draw-ws (make-world LOW3 "" 0))
              (above
              (place-image (text "d" 15 ACTIVE-COLOR) 75 75
                                  (place-image (text "o" 15 ACTIVE-COLOR) 90 75
                                  (place-image (text "g" 15 ACTIVE-COLOR) 105 75
                                  (place-image (text "" 25 TYPING-COLOR) 300 600 SCENE))))
              (text "" 15 TYPING-COLOR)))


           
;;;;;;;;;;;;;;;;;;;on-tick function;;;;;;;;;;;;;;;;;;;

;Low -> Number
;Returns how many values LOW contains
(define (number-words low)
  (cond [(empty? low) 0]
        [else (+ 1 (number-words (rest low)))]))

(check-expect (number-words LOW1) 0)
(check-expect (number-words LOW2) 3)
 

;LOW Word -> Boolean
;Takes a LOW and #true if any two words have the same position, else #false
 (define (when-collision low word)
   (cond
     [(empty? low) #false]
     [else (and (= (+ (posn-y (word-position word)) 1)
           (posn-y (word-position (first low))))
        (> (posn-x (word-position word))
           ( - (posn-x (word-position (first low))) (string-length (word-vocab word))))
        (> (+ (posn-x (word-position (first low))) (string-length (word-vocab (first low))))
           (posn-x (word-position word))))]))
     
(check-expect (when-collision LOW1 WORD1) #false)
(check-expect (when-collision LOW3 WORD1) #false)
(check-expect (when-collision LOW5 WORD4) #true)
 
;LOW Word -> Boolean
;Returns #true if Word hits the bottom of screen (y = 39) or if any words have the same position
(define (collision? low word) 
    (cond [(empty? low) (= (posn-y (word-position word)) 39)]
          [else (if (when-collision low word)
                    #true
                    (collision? (rest low) word))]))

(check-expect (collision? LOW5 WORD6) #true)
(check-expect (collision? LOW1 WORD1) #false)
(check-expect (collision? LOW5 WORD4) #true)
(check-expect (collision? LOW2 WORD1) #false)

;LOW -> Word
;Changes (word-stuck) of first Word in LOW to #true
(define (stuck low)
  (make-word (word-vocab (first low)) STUCK-COLOR (word-position (first low)) #true))

(check-expect (stuck LOW2) (make-word "dog" "red" (make-posn 5 5) #true))

;LOW -> Word
;Moves the first word in the list down one tick (one grid, y-1)
(define (move low)
  (make-word (word-vocab (first low))
             (word-color (first low))
             (make-posn (posn-x (word-position (first low)))
                        (+ 1 (posn-y (word-position (first low)))))
             (word-stuck? (first low))))

(check-expect (move LOW2) (make-word "dog" "green" (make-posn 5 6) #false))

;LOW -> LOW
;Moves all words in the LOW down one tick (one grid, y-1)
(define (update-word-position low)
  (cond [(empty? low) empty]
   [else  (if (collision? (rest low) (first low))
              (cons (stuck low) (update-word-position (rest low)))
              (cons (move low) (update-word-position (rest low))))]))
 
(check-expect (update-word-position LOW1) empty)
(check-expect (update-word-position LOW3)
              (cons (make-word "dog" "green" (make-posn 5 6) #false) '()))
(check-expect (update-word-position LOW6)
              (cons (make-word "fish" "red" (make-posn 10 3) #true)
              (cons (make-word "fish" "green" (make-posn 8 5) #true)
              (cons (make-word "fish" "green" (make-posn 8 5) #false) '()))))


;;;;;Random;;;;;;;;;

;String->Number
;Returns a random number from 0 to (39-length of given string) and adds 1
(define (get-random-x phrase)
  (add1 (random (- 40 1 (string-length phrase)))))

(check-random (get-random-x "dog") (add1 (random 36)))

;String -> Word
;Returns a new Word using a given string
(define (get-random phrase)
  (make-word phrase
             ACTIVE-COLOR
             (make-posn (get-random-x phrase) 0)
             #false))

(check-random (get-random "whale")
              (make-word "whale" ACTIVE-COLOR (make-posn (get-random-x "whale") 0) #false))

;LOW -> LOW
;Adds a new random word to the given list of words
(define (add-random-word low)
     (cons (get-random (list-ref LOV (random (length LOV)))) low)) 

(check-random (add-random-word LOW3)
              (cons (get-random (list-ref LOV (random (length LOV)))) LOW3))
;WS->LOW
;Adds a random word to the list of words if the (world-tick) is even
(define (add-word ws)
 (cond [(even? (world-tick ws)) (add-random-word (world-low ws))]
       [else (world-low ws)]))

(check-random (add-word WS5) (world-low WS5))
(check-random (add-word WS6) (add-random-word (world-low WS6)))
 

;WS -> WS 
;Outputs a new world state with all values updated for a tick 
(define (update-WORLD ws)
  (make-world (update-word-position (add-word ws)) (world-key-input ws) (+ 1 (world-tick ws))))

(check-random (update-WORLD WS2) (make-world (update-word-position (add-word WS2)) "WORDS" 1))

;;;;stop-when;;;

;LOW -> Boolean
;Returns #true if any word hits another word at the row 0 (y=0), else #false
(define (list-game-over? low)
  (cond
    [(empty? low) #false]
    [else (and (collision? (rest low) (first low))
               (= (posn-y (word-position (first low))) 0))])) 

(check-expect (list-game-over? LOW1) #false)
(check-expect (list-game-over? LOW7) #true)
(check-expect (list-game-over? LOW6) #false)


;WS -> Boolean
;checks if a world state has a list of word that satisfy list-game-over
(define (game-over? ws)
  (list-game-over? (world-low ws)))

(check-expect (game-over? WS3) #false)
(check-expect (game-over? WS4) #true)

;;;on-key;;;

;String -> Image
;Returns given String in (text) format
(define (place-string s)
  (place-image (text s 15 TYPING-COLOR) 300 600 (empty-scene 600 630)))

(check-expect (place-string "dog")
              (place-image (text "dog" 15 TYPING-COLOR) 300 600 (empty-scene 600 630)))

;WS String -> WS
;Returns WS with (world-key-input) and (world-low) updated based on given user input
(define (handler sw string)
  (cond [(string=? string "\b") (delete sw)]
        [(string=? string "\r") (enter sw)]
        [ (and(string-alphabetic? string)
              (<= (string-length string) 1))
              (write sw string)]
        [else sw]))

(check-expect (handler WS1 "\b")
              (make-world '() "" 0))
(check-expect (handler WS2 "\b")
              (make-world '() "WORD" 0))
(check-expect (handler WS1 "\r")
              (make-world '() "" 0))
(check-expect (handler WS2 "\r")
              (make-world '() "" 0))
(check-expect (handler WS1 "g")
              (make-world '() "g" 0))
(check-expect (handler WS2 "P")
              (make-world '() "WORDSP" 0))
(check-expect (handler WS1 "tab") WS1)
(check-expect (handler WS2 "left") WS2)

;WS String -> WS
;Adds String onto the end of (world-key-input)
(define (write ws string)
  (make-world (world-low ws)
              (string-append (world-key-input ws) string)
              (world-tick ws)))
 
(check-expect (write WS1 "g")
              (make-world '() "g" 0))
(check-expect (write WS2 "l")
              (make-world '() "WORDSl" 0))

;WS -> WS
;Returns given WS with one string removed from (word-key-input); if field is empty, returns empty
(define (delete ws)
  (make-world (world-low ws)
              (if (= (string-length (world-key-input ws)) 0)
                  ""
                  (substring (world-key-input ws) 0 ( - (string-length (world-key-input ws)) 1)))
              (world-tick ws)))
 
(check-expect (delete WS1) WS1)
(check-expect (delete WS2)
              (make-world '() "WORD" 0))

;WS -> WS
;Removes all instances of (world-key-input) from (world-low) and resets key-input to an empty string
(define (enter ws)
  (make-world
   (remove-word-state (world-low ws) (world-key-input ws))
   ""
   (world-tick ws)))
  
(check-expect (enter WS1) WS1)
(check-expect (enter WS2) WS1)
(check-expect (enter WS3)
              (make-world (cons (make-word "cat" "green" (make-posn 6 7) #true)
                          (cons (make-word "fish" "green" (make-posn 8 4) #false) '())) "" 0))


;LOW  -> LOW
;Returns LOW with words that have same (word-vocab) as String removed
(define (remove-word-state low string)
  (cond [(empty? low) empty]
        [else (if (string=? (word-vocab (first low)) string)
                  (remove-word-state (rest low) string)
                  (cons (first low) (remove-word-state (rest low) string)))]))

(check-expect (remove-word-state LOW2 "cat")
              (cons (make-word "dog" "green" (make-posn 5 5) #false)
              (cons (make-word "fish" "green" (make-posn 8 4) #false) '())))
(check-expect (remove-word-state '() "dog") '())
(check-expect (remove-word-state LOW2 "suspicious")
              LOW2)
(check-expect (remove-word-state (cons WORD1 (cons WORD1 '())) "dog") '())
              
   
;Number -> Number
;Returns final score of game, inverse frequency * final tick score of game
(define (main frequency)
  (* (/ 1 frequency) (world-tick
          (big-bang WS1
    (to-draw draw-ws)      
    (on-tick update-WORLD frequency)
    (on-key handler)
    (stop-when game-over?)))))
