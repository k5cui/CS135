;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 3
;; ************************************************************************
;;

;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; A Rating is an Int
;; requires: Int is either -1 or 1
;; A Pref-v is a (list Int Int Int Int Int Int Int Int)

;; (find-preference Rating Movie-fv) produces a Pref-v
;; (i.e. a preference vector), which will be an eight-element list that consists
;; of the sum of the scores for each of the eight genres. 
;; find-preference: (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: both lists are the same size
;; both lists are non-empty
;; example:
(check-expect (find-preference (list 1)
(list (list 1 1 1 0 0 0 0 0))) (list 1 1 1 0 0 0 0 0))

(define ending-list (list 0 0 0 0 0 0 0 0))

(define (listsum list1 list2)
  (cond [(empty? list1) empty]
        [else (cons (+ (first list1)(first list2))
                    (listsum (rest list1) (rest list2)))]))

(define (neglist list)
  (cond [(empty? list) empty]
        [else (cons (* (first list) -1) (neglist (rest list)))]))

(define (find-preference rating-list movie-types)
  (cond [(empty? rating-list) ending-list]
        [(= (first rating-list) 1)
         (listsum (first movie-types)
                  (find-preference (rest rating-list) (rest movie-types)))]
        [(= (first rating-list) -1)
         (listsum (neglist (first movie-types))
                  (find-preference (rest rating-list) (rest movie-types)))]))

;; tests:
(check-expect (find-preference (list -1)
                               (list (list 1 1 1 0 0 0 0 0)))
              (list -1 -1 -1 0 0 0 0 0))
(check-expect (find-preference (list 1 1 -1)
                               (list (list 1 1 0 0 0 0 0 1)
                                     (list 1 1 1 0 0 0 0 0)
                                     (list 1 1 1 1 0 0 0 0)))
              (list 1 1 0 -1 0 0 0 1))


(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)

(define (dot-product list1 list2)
  (cond [(empty? list1) 0]
        [else (+ (* (first list1) (first list2))
               (dot-product (rest list1) (rest list2)))]))

(define (max-score pref-score movie-list)
  (cond [(empty? (rest movie-list))
         (dot-product pref-score (movie-genres (first movie-list)))]
        [else (max (dot-product pref-score (movie-genres (first movie-list)))
                   (max-score pref-score (rest movie-list)))]))

;; (suggestions pred-score movie-list): produces the title of the movie with
;; the highest preference score.
;; suggestions: Pref-v (listof Movie) -> Str
;; requires: (listof Movie) is non-empty
;; example:
(check-expect (suggestions (list 2 1 0 -1 0 0 0 0)
(list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
(make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
(make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0)))) "The Meg")

(define (suggestions pref-score movie-list)
  (cond [(= (dot-product pref-score (movie-genres (first movie-list)))
            (max-score pref-score movie-list))
         (movie-title (first movie-list))]
        [else (suggestions pref-score (rest movie-list))]))

;; tests:
(check-expect (suggestions
(list 2 1 0 -1 0 0 0 0)
(list (make-movie "The Meg" (list 1 0 0 6 1 0 0 0))
(make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
(make-movie "A Star is Born" (list 8 0 0 1 0 1 0 0)))) "A Star is Born")

(check-expect (suggestions
(list 0 1 0 -1 0 0 0 10)
(list (make-movie "The Story" (list 1 0 0 0 1 0 1 2))
(make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
(make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0)))) "The Story")

(check-expect (suggestions
(list 3 1 0 -1 0 0 0 0)
(list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
      (make-movie "Smallfoot" (list 1 1 1 0 0 0 0 0))
      (make-movie "A Star is Born" (list 0 0 0 0 0 1 0 0)))) "Smallfoot")
