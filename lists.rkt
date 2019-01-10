;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 3
;; ************************************************************************
;;

;; (absolutely-odd intlist) produces the sum of all the absolute values of the
;; odd integers in a list of integers
;; absolutely-odd: (listof Int) -> Nat
;; example:
(check-expect (absolutely-odd (cons 1 (cons 2 (cons 3 empty)))) 4)

(define (absolutely-odd intlist)
  (cond [(empty? intlist) 0]
        [else (cond [(= (modulo(first intlist) 2) 0)
                     (absolutely-odd (rest intlist))]
                    [else (+ (abs (first intlist))
                             (absolutely-odd (rest intlist)))])]))

;; tests: 
(check-expect (absolutely-odd (cons 1 (cons 2 (cons -3 empty)))) 4)
(check-expect (absolutely-odd empty) 0)
(check-expect (absolutely-odd (cons 1 empty)) 1)

;; (spiraling? intlist) determines if a list of integers is a spiraling list
;; where the list alternates between positive and negative integers and the
;; absolute value of the integers are strictly increasing
;; spiraling?: (listof Int) -> Bool
;; example:
(check-expect (spiraling? (cons 1 (cons -2 (cons 3 empty)))) true)

(define (spiraling? intlist)
  (cond [(empty? intlist) true]
        [(empty? (rest intlist)) true]
        [(and (< (abs (first intlist)) (abs (first (rest intlist))))
              (< (* (first intlist) (first (rest intlist))) 0))
         (spiraling? (rest intlist))]
        [else false]))

;; tests:
(check-expect (spiraling? (cons -1 (cons 2 (cons -3 empty)))) true)
(check-expect (spiraling? (cons -1 (cons -2 (cons 3 empty)))) false)
(check-expect (spiraling? (cons 1 (cons 2 (cons -3 empty)))) false)
(check-expect (spiraling? (cons -3 (cons -2 (cons 3 empty)))) false)
(check-expect (spiraling? (cons -3 (cons 2 (cons -3 empty)))) false)
(check-expect (spiraling? empty) true)

;; (terms poslist) determines the number of terms in a list
;; terms: (listof Num) -> Nat
;; requires: poslist contains only positive values
;; example:
(check-expect (terms (cons 1 (cons 2 (cons 3 empty)))) 3)

(define (terms poslist)
  (cond [(empty? poslist) 0]
        [else (+ 1 (terms (rest poslist)))]))

;; (product poslist) determines the product of terms in a list
;; product: (listof Num) -> Num
;; requires: poslist contains only positive values
;; example:
(check-expect (product (cons 1 (cons 2 (cons 3 empty)))) 6)

(define (product poslist)
  (cond [(empty? poslist) 1]
        [else (* (first poslist) (product(rest poslist)))]))

;; (geometric-mean poslist) determines the geometric means of terms in a list
;; geometric-mean: (listof Num) -> Num
;; requires: poslist contains only positive values
;; example:
(check-within (geometric-mean (cons 1 (cons 2 (cons 3 empty)))) 1.8171 0.0001)

(define (geometric-mean poslist)
  (expt (product poslist) (/ 1 (terms poslist))))

;; tests:
(check-within (geometric-mean (cons 1.5 (cons 2 (cons 3 empty)))) 2.0800 0.0001)
(check-within (geometric-mean (cons 1 (cons 1 (cons 1 empty)))) 1 0.0001)
(check-within (geometric-mean (cons 1.5 empty)) 1.5 0.0001)
(check-within (geometric-mean (cons 1.5 (cons 2 (cons 1 empty)))) 1.4422 0.0001)
