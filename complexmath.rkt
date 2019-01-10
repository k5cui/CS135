;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 03, Problem 2
;; ************************************************************************
;;

;; (posn-mult posn1 posn2) produces the multiplication of 2 posn values
;; posn-mult: Posn Posn -> Posn
;; example:
(check-expect (posn-mult (make-posn 1 2) (make-posn 2 1)) (make-posn 0 5))

(define (posn-mult posn1 posn2)
  (make-posn
    (- (* (posn-x posn1) (posn-x posn2)) (* (posn-y posn1)(posn-y posn2)))
    (+ (* (posn-x posn1) (posn-y posn2)) (* (posn-x posn2)(posn-y posn1)))))

;; tests:
(check-expect (posn-mult (make-posn 3 2) (make-posn 2 1)) (make-posn 4 7))
(check-expect (posn-mult (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (posn-mult (make-posn 1 1) (make-posn 1 2)) (make-posn -1 3))
(check-expect (posn-mult (make-posn 0 2) (make-posn 2 -1)) (make-posn 2 4))
(check-expect (posn-mult (make-posn 1 6) (make-posn -2 -1))(make-posn 4 -13))
(check-expect (posn-mult (make-posn -1 6) (make-posn 2 -1))(make-posn 4 13))
(check-expect (posn-mult (make-posn -1 -4) (make-posn 2 -1))(make-posn -6 -7))

;; (posn-div posn1 posn2) produces the multiplication of 2 posn values
;; posn-div: Posn Posn -> Posn
;; requires: x of posn2 and y of posn2 cannot both be 0
;; example: 
(check-expect (posn-div (make-posn 1 2) (make-posn 1 2)) (make-posn 1 0))

(define (posn-div posn1 posn2)
  (make-posn
   (/ (+ (* (posn-x posn1) (posn-x posn2)) (* (posn-y posn1) (posn-y posn2)))
      (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))
   (/ (- (* (posn-y posn1) (posn-x posn2)) (* (posn-x posn1) (posn-y posn2)))
      (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))))

;; tests:
(check-expect (posn-div (make-posn 3 2) (make-posn 2 1))(make-posn 1.6 0.2))
(check-expect (posn-div (make-posn 1 1) (make-posn 1 2))(make-posn 0.6 -0.2))
(check-expect (posn-div (make-posn 0 2) (make-posn 2 -1))(make-posn -0.4 0.8))
(check-expect (posn-div (make-posn 1 6) (make-posn -2 -1))(make-posn -1.6 -2.2))
(check-expect (posn-div (make-posn -1 6) (make-posn 2 -1))(make-posn -1.6 2.2))
(check-expect (posn-div (make-posn -1 -4) (make-posn 2 -1))(make-posn 0.4 -1.8))

;; (posn-reciprocal posn1) produces the multiplication of 2 posn values
;; posn-div: Posn -> Posn
;; requires: x of posn1 and y of posn1 cannot both be 0
;; example:
(check-expect (posn-reciprocal (make-posn 1 2)) (make-posn 0.2 -0.4)) 
               
(define (posn-reciprocal posn1)
  (make-posn
   (/ (posn-x posn1) (+ (sqr (posn-x posn1)) (sqr (posn-y posn1))))
   (* -1 (/ (posn-y posn1) (+ (sqr (posn-x posn1)) (sqr (posn-y posn1)))))))

;; tests:
(check-expect (posn-reciprocal (make-posn 1 2)) (make-posn 0.2 -0.4))
(check-expect (posn-reciprocal (make-posn 0 2)) (make-posn 0 -0.5))
(check-expect (posn-reciprocal (make-posn 1 -3)) (make-posn 0.1 0.3))
(check-expect (posn-reciprocal (make-posn -1 -2)) (make-posn -0.2 0.4))
(check-expect (posn-reciprocal (make-posn 1 0)) (make-posn 1 0))
(check-expect (posn-reciprocal (make-posn -1 2)) (make-posn -0.2 -0.4)) 

