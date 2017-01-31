;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lecture russian doll|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;Russian doal is one of:
;-symbol
;-(make-shell symbol RD)

(define-struct shell [color inner])

;interp:
;just a symbol means the center doll
;otherwise we have a shell sontaining 1+ dolls

#;
(define (rd-tmpl rd )
  (cond [(symbol? rd) ...]
        [(shell? rd...(shell-color rd)...
                   ...(rd-tmpl (shell-inner rd))...)])) 

(define DOLL1 'green)
(define DOLL3 (make-shell 'blue (make-shell 'green 'red)))
(define DOLL6
  (make-shell 'red
              (make-shell 'green
                          (make-shell 'blue
                                      (make-shell 'purple
                                                  (make-shell 'orange 'black))))))
;RD-> Number
;how many dolls we have
(check-expect (count-dolls DOLL6) 6)

(define (count-dolls rd)
  (cond [(symbol? rd) 1]
        [(shell? rd) (+ 1 (count-dolls (shell-inner rd)))]))


;RD->Symbol
;Give the color of the outrermost doll

(define (outer-color rd)
  (cond [(symbol? rd) rd]
        [(shell? rd) (shell-color rd)]))

;RD->Symbol
;Give the color of the innerrmost doll

(check-expect (inner-color DOLL1) 'green)
(check-expect (inner-color DOLL6) 'black)
        
(define (inner-color rd)
  (cond [(symbol? rd) rd]
        [(shell? rd) (inner-color (shell-inner rd))]))

(define (draw-one-doll size color)
  (overlay/xy (circle size 'solid color)
              (- (/ size 2)) size
              (circle (* size 1.5) 'solid color)))

(define (doll-size d)
  (* 20 (count-dolls d)))

;RD -> Image
;Draw a whole stack of dolls

(define (draw-dolls rd)
  (cond [(symbol? rd) (draw-one-doll (doll-size rd) rd)]
        [(shell? rd) (overlay (draw-dolls (shell-inner rd))
                             (draw-one-doll (doll-size rd) (shell-color rd)))])) 

;A NN (Natural Number) is one of:
;-0
;-(add1 NN)

#;

(define (nn-tmpl x)
  (cond [(zero? 0) ...]
  [else ... (nn-tmpl (sub1 x))...]))

;NN->NN
;Raise x to the yth power.

(check-expect (pow 3 2) 9)
(check-expect (pow 2 9) 512)
(check-expect (pow 8 0) 1)

(define (pow x y)
  (cond [(zero? y) 1]
        [else (* x (pow x (sub1 y)) )]))

(define DOLL6
  (make-shell 'red
              (make-shell 'green
                          (make-shell 'blue
                                      (make-shell 'purple
                                                  (make-shell 'orange 'black))))))

;; find-doll rd color
(define (find-doll rd color n)
              ;; base condition
   (cond [(symbol? rd)
                     (cond [(eq? (shell-color rd) color) 1]
                               [else 0])]
              ;; recursive part
             [else (cond [(eq? (shell-color rd) color) n] ;; if current body matches the color
                               [else (find-doll (shell-inner rd) color (+ n 1))] )])) ;; if not, check it's inner shell


(find-doll DOLL6 'blue 1)



(symbol? 'green)
(symbol=? 'green 'green)
(symbol=? 'green 'blue)
(circle 50 'solid 'blue)

