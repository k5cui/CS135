;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 01, Problem 3
;; ***************************************************
;;

(define kmpermile 1.609344)

(define litrepergallon 3.785411784)

(define mLperthimble 2.1)

(define rodperchain 4)

(define yardperrod 5.5)

(define yardpermile 1760)

(define mLperL 1000)

(define (mpg->lp100km mpg)
  (/ 1 (* mpg (/ kmpermile 100)
          (/ 1 litrepergallon))))

(define (mpg->cpt mpg)
  (/ (/ (* (/ (/ mpg litrepergallon)
              (/ mLperL mLperthimble))
           yardpermile) yardperrod) rodperchain))