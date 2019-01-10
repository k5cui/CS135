;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 01, Problem 2
;; ***************************************************
;;

(define (volume r h)
  (/(* pi (sqr r) h) 3))

(define (escape M r)
  (sqrt (/ (* 2 6.674e-11 M) r)))

(define (Stirling n)
  (* (sqrt (* 2 pi n))
     (expt (/ n e) n)))

(define (payment P r n)
  (* P (/ (* r (expt (+ 1 r) n))
          (+ (expt (+ 1 r) n) 1))))

(define (partition-size-approximation n)
(* (/ 1 (* 4 n (sqrt 3)))
   (exp (* pi (sqrt (/ (* 2 n) 3))))))
