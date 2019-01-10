;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname score) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 02, Problem 1
;; ************************************************************************
;;

;; base point values for the number of lines cleared
(define oneline-p 40)
(define twoline-p 100)
(define threeline-p 300)
(define fourline-p 1200)

;; (oneline n) produces the number of points
;; when one line is cleared at level n
;; oneline: Num -> Num
;; requires: 0 <= n
;; example:
(check-expect (oneline 10) 440)
 
(define (oneline n) (* (+ n 1) oneline-p))

;; tests:
(check-expect (oneline 0) 40)
(check-expect (oneline 99) 4000)

;; (twoline n) produces the number of points
;; when two lines are cleared at level n
;; twoline: Num -> Num
;; requires: 0 <= n
;; example:
(check-expect (twoline 10) 1100)

(define (twoline n) (* (+ n 1) twoline-p))

;; tests:
(check-expect (twoline 0) 100)
(check-expect (twoline 99) 10000)

;; (threeline n) produces the number of points
;; when three lines are cleared at level n
;; threeline: Num -> Num
;; requires: 0 <= n
;; example:
(check-expect (threeline 10) 3300)

(define (threeline n) (* (+ n 1) threeline-p))

;; tests:
(check-expect (threeline 0) 300)
(check-expect (threeline 99) 30000)

;; (fourline n) produces the number of points
;; when four lines are cleared at level n
;; fourline: Num -> Num
;; requires: 0 <= n
;; example:
(check-expect (fourline 10) 13200)

(define (fourline n) (* (+ n 1) fourline-p))

;; tests:
(check-expect (fourline 0) 1200)
(check-expect (fourline 99) 120000)

;; (tetris score a b) produces the number of points
;; when b lines are cleared at level a
;; tetris_score: Num Num -> Num
;; requires: 0 <= a
;;           1 <= b
;; example:
(check-expect (tetris-score 20 2) 2100)

(define (tetris-score a b) (cond [(= b 1) (oneline a)]
                                 [(= b 2) (twoline a)]
                                 [(= b 3) (threeline a)]
                                 [(= b 4) (fourline a)]
                                 [(> b 4) 0]))

;; tests:
(check-expect (tetris-score 5 1) 240)
(check-expect (tetris-score 6 2) 700)
(check-expect (tetris-score 3 3) 1200)
(check-expect (tetris-score 7 4) 9600)
(check-expect (tetris-score 4 5) 0)
