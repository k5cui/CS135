;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 09, Problem 3
;; ************************************************************************
;;

(define test-a (list 2 4 6 8 10 12))
(define test-b (list 1 11 33 67 113))
(define test-c (list 2 4 8 16 32 64))
(define test-d (list 1 1 2 3 5 8 13))
(define test-e (list 0 1 1 2 3 5 8))
(define test-f (list 1 1 3 5 11 21 43))
(define test-g (list 2 0 0 0 0 0))
(define test-h (list 3 9 27 81))
(define test-i (list 0 0 0 0 0))

;; (list=? list-one list-two) checks if 2 lists are equal
;; list=?: (listof Any) (listof Any) -> Bool
;; example:
(check-expect (list=? '(a b c) '(a b c)) true)

(define (list=? list-one list-two)
  (cond [(empty? list-one) true]
        [(equal? (first list-one) (first list-two))
         (list=? (rest list-one) (rest list-two))]
        [else false]))

;; (solution? funct list) checks if a function applied to a sequence is equal to
;; the given list
;; solution?: (Nat -> Any) (listof Any) -> Bool
;; example:
(check-expect (solution? (lambda (i) (* 2 i)) (list 0 2 4 6 8 10 12 14)) true)

(define (solution? funct list)
  (list=? (build-list (length list) funct) list))

(check-expect (solution? (lambda (i) (+ i 1)) (list 1 2 3 4 5 6)) true)
(check-expect (solution? (lambda (i) i) (list 1 2 3 4 5 6)) false)
(check-expect (solution? (lambda (i) i) empty) true)

;; (guess-quadratic lon) guesses that the number sequence lon has a
;; quadratic solution and produces the guess.
;; guess-quadratic: (listof Int) -> (Nat -> Int)
;; example:
(check-expect (solution? (guess-quadratic test-a) test-a) true)
(check-expect (solution? (guess-quadratic test-b) test-b) true)
(check-expect (solution? (guess-quadratic test-c) test-c) false)

(define (guess-quadratic lon)
  (cond [(empty? lon) (lambda (i) 0)]
        [(= (length lon) 1) (lambda (i) (first lon))]
        [(= (length lon) 2) (lambda (i) (+ (first lon) (* i (- (second lon)
                                                               (first lon)))))]
        [else (local [(define x (first lon))
                      (define y (second lon))
                      (define z (third lon))]
                (lambda (i) (+ (* (- (/ (+ z x) 2) y) (sqr i))
                               (* (- (* 2 y) (* 1.5 x) (/ z 2)) i)
                               x)))]))

;; (try-quadratic lon) tries solving the number sequence lon with
;; a quadratic. It produces the solution if it works and empty
;; if it doesn't.
;; try-quadratic: (listof Int) -> (anyof (Nat -> Int) empty)
;; example:
(check-expect ((try-quadratic test-a) 6) 14)
  
(define (try-quadratic lon)
  (local [(define quadratic (guess-quadratic lon))]
    (cond [(solution? quadratic lon)
           quadratic]
          [else empty])))

;; (guess-recursive lon) guesses that the number sequence lon has a
;; recursive solution and produces the guess.
;; guess-recursive: (listof Int) -> (Nat -> Int)
;; example:
(check-expect (solution? (guess-recursive test-d) test-d) true)

(define (guess-recursive lon)
  (lambda (i)
    (cond [(empty? lon) 0]
          [(< i (length lon)) (list-ref lon i)]
          [(= (length lon) 3) ...]
          [(and (= (first lon) 0) (= (second lon) 0)) 0]
          [(= (- (sqr (second lon)) (* (first lon) (third lon))) 0)
           (* (first lon) (expt (/ (second lon) (first lon)) i))]
          [else
           (local [(define a (/ (- (* (second lon) (third lon))
                                   (* (first lon) (fourth lon)))
                                (- (sqr (second lon))
                                   (* (first lon) (third lon)))))
                   (define b (/ (- (sqr (third lon))
                                   (* (second lon) (fourth lon)))
                                (- (* (first lon) (third lon))
                                   (sqr (second lon)))))
                   (define (guess-recursive-acc num-one num-two index)
                     (cond [(= index 2) (+ (* a num-one) (* num-two b))]
                           [else (guess-recursive-acc (+ (* a num-one)
                                                         (* num-two b))
                                                      num-one
                                                      (sub1 index))]))]
             (guess-recursive-acc (second lon) (first lon) i))])))

;; (try-recursive lon) tries solving the number sequence lon with
;; recursion. It produces the solution if it works and empty
;; if it doesn't.
;; try-recursive: (listof Int) -> (anyof (Nat -> Int) empty)
;; example:
(check-expect ((try-recursive test-d) 100) 573147844013817084101)
             
(define (try-recursive lon)
  (local [(define recursive (guess-recursive lon))]
    (cond [(solution? recursive lon)
           recursive]
          [else empty])))

;; tests
(check-expect ((try-recursive test-h) 3) 81)
(check-expect ((try-recursive test-h) 4) 243)
(check-expect ((try-recursive test-i) 100) 0)

;; (solve lon) tries both the quadratic and recursive patterns, in that order,
;; and returns a solution, if one exists. Otherwise return the empty list
;; solve: (listof Int) -> (anyof (Nat -> Int) empty)

(define (solve lon)
  (local [(define solutions
            (list (list (lambda (i) (solution? (guess-quadratic i) i))
                        (lambda (i) (guess-quadratic i)))
                  (list (lambda (i) (solution? (guess-recursive i) i))
                        (lambda (i) (guess-recursive i)))))
          (define (solve-help function)
            (cond [(empty? function) empty]
                  [((first (first function)) lon)
                   ((second (first function)) lon)]
                  [else (solve-help (rest function))]))]
    (solve-help solutions)))