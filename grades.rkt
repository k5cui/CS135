;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ********************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 01, Problem 4
;; ********************************************
;;

(define m1weight 0.1)

(define m2weight 0.2)

(define assignmentweight 0.2)

(define finalweight 0.45)

(define clickerscore 100)

(define clickerweight 0.05)

(define gradeneeded 60)

(define (final-cs135-grade midterm1 midterm2 finalexam assignments)
  (+ (* clickerscore clickerweight)
     (* midterm1 m1weight)
     (* midterm2 m2weight)
     (* finalexam finalweight)
     (* assignments assignmentweight)))

(define (cs135-final-exam-grade-needed midterm1 midterm2 assignments)
  (/ (- gradeneeded (+ (* clickerscore clickerweight)
                       (* midterm1 m1weight)
                       (* midterm2 m2weight)
                       (* assignments assignmentweight))) finalweight))