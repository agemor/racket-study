;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 2 pracitce on my own 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;example 10/ Define distance0.
;The function consumes an instance of 3D and computes
;the distance from the given point to the origin of the space.
;Hint: Your math friend reminds you that the distance is computed
;as the square root of the sum of the squares of the coordinates.

(define-struct 3d [x y z])
(define 3d-a (make-3d 0 0 1))
(define 3d-b (make-3d 2 3 4))

(define (distance0 a)
  (sqrt(+ (sqr (3d-x a)) (sqr (3d-y a)) (sqr (3d-z a)))))

(distance0 3d-a)

;example 11/ Define the function birthday which takes an Animal and returns
;a new Animal with the same contents except with 1 added to its age.

(define-struct animal [name species year breakfast-time dinner-time])

(define animalA (make-animal "Leo" "Lion" 10 12 18))
(animal-name animalA)
(animal? animalA)

(define (birthday X)
  (make-animal (animal-name X)
               (animal-species X)
               (+ (animal-year X) 1)
               (animal-breakfast-time X)
               (animal-dinner-time X)))
  
(birthday animalA)

;---boolean------------------------
(define (<3 x) #true)
(<3 4)

(define (function x) (< x 3))
(function 2)

;both of them results are boolean
;----------------------------------

;exercise 12/ To ensure the safety of zoo visitors,
;define a function that consumes an Animal and the current hour
;and returns whether it’s mealtime.

(define (Mealtime? x current-hour)
  (cond [ (= (animal-breakfast-time x) current-hour) "mealtime"]
        [ (= (animal-dinner-time x) current-hour) "mealtime"]
        [ else "is-not-mealtime"]))


(Mealtime? animalA 13)
(Mealtime? animalA 12)
(Mealtime? animalA 18)


;exercise 13/  In preparation for next April Fool’s Day,
;the system manager of the zoo wants you to define a function
;that takes an Animal and returns a new Animal with the same data contents
;except with its age converted to dog years.
;Note: there are 7 dog years in 1 human year.

(define (DogAge x)
  (make-animal (animal-name x)
               (animal-species x)
               (* (animal-year x) 7)
               (animal-breakfast-time x)
               (animal-dinner-time x)))
(DogAge animalA)

;exercise 14/ Sometimes the animals in the zoo have babies!
;Define a function which consumes an Animal and producesa new animal
;representing the new baby. The baby should have the same species
;and feeding schedule as the parent but a new name.
;The zoo staff are not very creative so the new name should be the parent’s name
;with the suffix “Jr.” at the end. For define, an animal named Bob would have a child named Bob Jr

(define (BabyName x)
  (make-animal (string-append (animal-name x) " Jr.")
               (animal-species x)
               0
               (animal-breakfast-time x)
               (animal-dinner-time x)))

(BabyName animalA)

;exercise 15/ Define a function that given a Sale
;outputs the cost of the item after the sale is applied.????

(define-struct item [tag price])
(define itemA (make-item "bag" 20))


(define-struct sale [item price percent-sale])
(define saleA (make-sale "bag" 100 0.1)) ;;  ><><><><>< how to incorporate

(define (FinalCost x)
  (* (sale-price x) (- 1 (sale-percent-sale x))))

(FinalCost saleA)









