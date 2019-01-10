;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a08helpers.rkt")
(require "ranking.rkt")

;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 08, Problem 5
;; ************************************************************************
;;

(define my-employers
  '(("Ash" (("Pikachu" 3) ("Charizard" 2) ("Aipom" 2) ("Geodude" 1)))
    ("Brock" (("Steelix" 1) ("Geodude" 2) ("Aipom" 2)))
    ("Misty" (("Staryu" 3) ("Psyduck" 2) ("Pikachu" 3)))
    ("Dawn" (("Aipom" 1)))
    ))

(define my-students
  '(("Pikachu" (("Ash" 3) ("Misty" 3)))
    ("Charizard" (("Ash" 2)))
    ("Aipom" (("Ash" 2) ("Dawn" 1) ("Brock" 2)))
    ("Geodude" (("Ash" 1) ("Brock" 2)))
    ("Steelix" (("Brock" 1)))
    ("Staryu" (("Misty" 3)))
    ("Psyduck" (("Misty" 2)))))

;; (match employers students) produces a list containing the best matches
;; of students and employers
;; match: (listof EmpRanking) (listof StdRanking) -> (listof (list EmpID StdID))
;; requires: all the EmpIDs in (listof EmpRanking) can be found in
;; (listof StdRanking)
;; example:
(check-expect (match my-employers my-students)
              '(("Dawn" "Aipom") ("Ash" "Geodude") 
                ("Brock" "Steelix") ("Misty" "Psyduck")))

(define (match emp-list stu-list) 
  (local [;; (remove-extra lst) takes the best preference score for a an student
          ;; and employer and expunges them from the rest of the list
          ;; remove-extra: (listof FlatRanking) -> (listof EmpID StdID)

          (define (remove-extra lst)
            (cond [(empty? lst) empty]
                  [else (cons (list (first (first lst)) (second (first lst)))
                              (remove-extra (expunge lst (first (first lst))
                                                     (second (first lst)))))]))
          ;; (sort-pref lst) sorts a list of FlatRanking based on their
          ;; preference score
          ;; sort-pref: (listof FlatRanking) -> (listof FlatRanking)
          
          (define (sort-pref lst)
            (quicksort lst (lambda (x y) (< (third x) (third y)))))]
    
    (remove-extra (sort-pref
                   (map (lambda (a)
                          (list (first a)
                                (second a)
                                (+ (third a)
                                   (find-pref stu-list (second a)(first a))
                                   (random-epsilon (first a)(second a)))))
                        (unfold emp-list))))))

;; tests:
(check-expect (match 
'(("Ash" (("Pikachu" 3) ("Charizard" 2) ("Aipom" 2) ("Geodude" 1)))
    ("Brock" (("Steelix" 2) ("Geodude" 1) ("Aipom" 2)))
    ("Misty" (("Staryu" 3) ("Psyduck" 2) ("Pikachu" 3)))
    ("Dawn" (("Aipom" 1))))
'(("Pikachu" (("Ash" 3) ("Misty" 3)))
    ("Charizard" (("Ash" 2)))
    ("Aipom" (("Ash" 2) ("Dawn" 1) ("Brock" 2)))
    ("Geodude" (("Ash" 1) ("Brock" 1)))
    ("Steelix" (("Brock" 2)))
    ("Staryu" (("Misty" 3)))
    ("Psyduck" (("Misty" 2)))))
              '(("Dawn" "Aipom") ("Ash" "Geodude") 
                ("Misty" "Psyduck") ("Brock" "Steelix")))

