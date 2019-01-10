;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname park-rentals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 2
;; ************************************************************************
;;

(define-struct bicycle (make model serial-number))
;; A Bicycle is a (make-bicycle Str Str Nat)

(define-struct boat (type serial-number))
;; A Boat is a (make-boat Sym Nat)
;; requires: type is 'paddle-boat or 'canoe

(define-struct horse (name serial-number capacity stamina))
;; A Horse is a (make-horse Str Nat Nat Num)
;; requires: stamina > 0, capacity > 0

;; A Rental is (anyof Bicycle Boat Horse)
;; rental-template: Rental -> any
(define (rental-template info)
  (cond [(bicycle? info)
         (... (bicycle-make info) ...
              (bicycle-model info) ...
              (bicycle-serial-number info) ...)]
        [(boat? info)
         (... (boat-type info) ...
              (boat-serial-number info) ...)]
        [(horse? info)
         (... (horse-name info) ...
              (horse-serial-number info) ...
              (horse-capacity info) ...
              (horse-stamina info) ...)])) 

;; useful constants
(define bike-rental-cost 20)
(define bike-capacity 1)
(define boat-rental-cost 30)
(define canoe-capacity 2)
(define paddle-boat-capacity 3)
(define horse-base-price 30)
(define horse-additional-cost 10)
(define horse-carrying-power 3)

;; (rental-id rented) combined some rented object and produces its serial number
;; rental-id: Rental -> Num
;; example:
(check-expect (rental-id (make-bicycle "Banana" "Orange" 123)) 123)

(define (rental-id rented)
  (cond [(bicycle? rented)
         (bicycle-serial-number rented)]
        [(boat? rented)
         (boat-serial-number rented)]
        [(horse? rented)
         (horse-serial-number rented)]))

;; tests:
(check-expect (rental-id (make-boat 'canoe 5)) 5)
(check-expect (rental-id (make-horse "Wonny" 543 5 6)) 543)

;; (rental-ok? rented people time) determines if a rental is valid based on the
;; type of rental, number of people, the time duration of the rental
;; rental-ok?: Rental Nat Num -> Bool
;; requires: time > 0
;; example:
(check-expect (rental-ok? (make-bicycle "Banana" "Orange" 123) 1 10) true)

(define (rental-ok? rented people time)
  (cond [(bicycle? rented)
         (= bike-capacity people)]
        [(boat? rented)
         (cond [(symbol=? (boat-type rented) 'canoe)
                (<= people canoe-capacity)]
               [else (<= people paddle-boat-capacity)])]  
        [(horse? rented)
         (and (<= people (horse-capacity rented))
              (<= time (horse-stamina rented)))]))


(check-expect (rental-ok? (make-bicycle "Banana" "Orange" 123) 2 10) false)
(check-expect (rental-ok? (make-boat 'canoe 5) 2 4) true)
(check-expect (rental-ok? (make-boat 'canoe 5) 3 4) false)
(check-expect (rental-ok? (make-boat 'paddle-boat 5) 3 4) true)
(check-expect (rental-ok? (make-boat 'paddle-boat 5) 4 4) false)
(check-expect (rental-ok? (make-horse "Wonny" 543 5 6) 5 6) true)
(check-expect (rental-ok? (make-horse "Wonny" 543 5 6) 6 6) false)
(check-expect (rental-ok? (make-horse "Wonny" 543 5 6) 5 7) false)

;; (rental-price rented people time) determines the cost of any given rental
;; based on its type, amount of people, and time duration
;; rental-price: Rental Nat Num -> Num
;; requires: time > 0
;;           (rental-ok? rented people time) must be true
;; example:
(check-expect (rental-price (make-bicycle "Banana" "Orange" 123) 1 10) 200)

(define (rental-price rented people time)
  (cond [(bicycle? rented)
         (* bike-rental-cost (ceiling time))]
        [(boat? rented)
         (* boat-rental-cost (ceiling time))]
        [(horse? rented)
         (cond [(> people 3)
                (* (+ horse-base-price (* (- people horse-carrying-power)
                                          horse-additional-cost))
                   (ceiling (* 2 time)))]
               [else (* horse-base-price (ceiling (* 2 time)))])]))

;; tests:
(check-expect (rental-price (make-boat 'canoe 5) 2 4) 120)
(check-expect (rental-price (make-boat 'paddle-boat 5) 3 4) 120)
(check-expect (rental-price (make-horse "Wonny" 543 5 6) 5 3) 300)
(check-expect (rental-price (make-horse "Wonny" 543 5 6) 5 3.1) 350)
(check-expect (rental-price (make-horse "Wonny" 543 5 6) 5 2.9) 300)
(check-expect (rental-price (make-horse "Wonny" 543 5 6) 5 2.4) 250)
(check-expect (rental-price (make-horse "Wonny" 543 5 6) 2 3) 180)

