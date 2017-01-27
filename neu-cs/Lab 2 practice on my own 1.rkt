;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 2 practice on my own 1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;Exercies 1

;A 3D is a (make-3d number number number)
(define-struct 3d [x y z])
;where x is the x-coordinate
;where y is the y-coordinate
;where z is the z-coordinate

;constructor: make-3d
(define 3d-A (make-3d 1 1 1))
;predicate: 3d?
(3d? 3d-A)
(check-expect (3d? 3d-A) #true)
;selector: 3d-x 3d-y 3d-z
(3d-x 3d-A)
(check-expect (3d-x 3d-A) 1)

;Exercise 1-2
;A TA is a (make-ta String String Number)
(define-struct ta [last given lab])
;where last is the TA's last name
;where given is the TA's first name
;where lab us the TA's lab number

;constructor: make-ta
;predicate: ta?
;selector: ta-last, ta-given, ta-lab

;Exercise 2-1/2-2
(define-struct item [tag price])
;consturctor: make-item
;predicate: item?
;selector: item-tag, item-price

(define-struct sale [item percent-off])
;constructor: make-sale
;predicate: sale?
;selector: sale-item, sale-percet-off

;Exercise 3
;A animal is a (make-animal string string number number number)
(define-struct animal [name species age breakfast-hour dinner-hour])
;where name is the name of the animal
;where species is the species of the animal
;where age is the age of the animal in years
;where breakfast-hour is when the animals eats its breakfast in 24/hour clock
;where dinner-hour is when the animals eats its dinner in 24/hour clock

;constructor: make-animal
(define AnimalA (make-animal "Leo" "lion" 20 8 24))
;predictor: animal?
;selector: animal-name, animal-species, animal-age, animal-breakfasthour, animal-dinnerhour


;Exercise 8

;sale-temp: sale->?

(define (sale-temp x)
  (...(item-temp (sale-item x))...
      (sale-percent-off x)...))

(define (item-temp x)
  (...(item-tag x)...
      (item-price x)...))

;Stepper

(define-struct employee [first last wage ssn])
(define empJohn (make-employee "John" "Wilson" 22 145))
(employee-last empJohn)

;;--1. paid-enough?: employee-> Boolean

(define (paid-enough? x)
  (> (employee-wage x) 10))

(check-expect (paid-enough? empJohn) #true)

;;--2. related? employee / string -> Boolean

(define (related? x last)
  (string=? (employee-last x) last ))

(check-expect (related? empJohn "Kim") #false)
(check-expect (related? (make-employee "Happy" "Kim" 30 125) "Kim") #true)




  