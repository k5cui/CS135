;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname laundry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 02, Problem 3
;; ************************************************************************
;;

;; (acceptable-to-wear/cond? smelly? clothing_type days)
;; determines if a piece of clothing is suitable to be worn again given
;; the smelliness smelly?, clothing type clothing_type, and time its been
;; in the hamper days
;; age: Bool Sym Num -> Bool
;; requires: clothing_type must be 'shirt or 'socks
;;           days > 0
;; example:
(check-expect (acceptable-to-wear/cond? true 'socks 8) false)

(define (acceptable-to-wear/cond? smelly? clothing_type days)
  (cond [smelly? false]
        [else (cond
                   [(symbol=? clothing_type 'shirt) (cond
                                          [(> days 2) (cond
                                                     [(< days 10) true]
                                                     [else false])]
                                          [else false])]
                   [(symbol=? clothing_type 'socks) (cond
                                          [(< days 4) true]
                                          [else false])])]))

;; tests:
(check-expect (acceptable-to-wear/cond? false 'shirt 1) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 2) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 5) true) 
(check-expect (acceptable-to-wear/cond? false 'shirt 10) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 11) false)
(check-expect (acceptable-to-wear/cond? false 'socks 1) true)
(check-expect (acceptable-to-wear/cond? false 'socks 4) false)
(check-expect (acceptable-to-wear/cond? false 'socks 6) false)
               
;; (acceptable-to-wear/cond? smelly? clothing_type days)
;; determines if a piece of clothing is suitable to be worn again given
;; the smelliness smelly?, clothing type clothing_type, and time its been
;; in the hamper days
;; age: Bool Sym Num -> Bool
;; requires: clothing_type must be 'shirt or 'socks
;;           days > 0
;; example:
(check-expect (acceptable-to-wear/bool? true 'shirt 3) false)

(define (acceptable-to-wear/bool? smelly? clothing_type days)
  (or (and (not smelly?) (symbol=? clothing_type 'shirt)
           (and (> days 2) (< days 10)))
      (and (not smelly?) (symbol=? clothing_type 'socks) (< days 4))))

;; tests:
(check-expect (acceptable-to-wear/bool? false 'shirt 1) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 2) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 6) true) 
(check-expect (acceptable-to-wear/bool? false 'shirt 10) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 13) false)
(check-expect (acceptable-to-wear/bool? false 'socks 2) true)
(check-expect (acceptable-to-wear/bool? false 'socks 4) false)
(check-expect (acceptable-to-wear/bool? false 'socks 7) false)