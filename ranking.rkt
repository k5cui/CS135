;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "provide.rkt")
(provide expunge)
(provide ranking-sort)
(provide unfold)
(provide find)
(provide find-pref)

;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 08, Problem 4
;; ************************************************************************
;;

;; An employer ID, EmpId, is a Str (e.g. "Google")
;; A student ID, StdId, is a Str (e.g. "Anna")

;; An Id is (anyof EmpId StdId)
;; A preference, Pref, is a Nat (e.g. 2)
;; requires: a preference is >= 1

;; An EmpRanking is (list EmpId (listof (list StdId Pref)))
;; requires: The list of preferences is in non-decreasing order
;; and non-empty.

;; A StdRanking is (list StdId (listof (list EmpId Pref)))
;; requires: The list of preferences is in non-decreasing order
;; and non-empty.

;; A Ranking is (anyof EmpRanking StdRanking)

(define my-employers
  '(("Ash" (("Pikachu" 3) ("Charizard" 2) ("Aipom" 2) ("Geodude" 1)))
    ("Brock" (("Steelix" 3) ("Geodude" 2) ("Aipom" 1)))
    ("Misty" (("Staryu" 3) ("Psyduck" 2) ("Pikachu" 3)))
    ("Dawn" (("Aipom" 1)))
    ))

(define my-students
  '(("Pikachu" (("Ash" 3) ("Misty" 3)))
    ("Charizard" (("Ash" 2)))
    ("Aipom" (("Ash" 2) ("Dawn" 1) ("Brock" 1)))
    ("Geodude" (("Ash" 1) ("Brock" 2)))
    ("Steelix" (("Brock" 3)))
    ("Staryu" (("Misty" 3)))
    ("Psyduck" (("Misty" 2)))))

;; A FlatRanking is a (list Str Str Num).
(define my-flat-employers
'(("Ash" "Pikachu" 3) ("Ash" "Charizard" 2)
("Ash" "Aipom" 2) ("Ash" "Geodude" 1)
("Brock" "Steelix" 3) ("Brock" "Geodude" 2)
("Brock" "Aipom" 1) ("Misty" "Staryu" 3)
("Misty" "Psyduck" 2) ("Misty" "Pikachu" 3)
("Dawn" "Aipom" 1)))

;; (expunge lofr rankg-id rankd-id) removes any FlatRankings containing
;; rankg-id or rankd-id from a list of FlatRankings
;; expunge: (listof FlatRanking) Str Str -> (listof FlatRanking)
;; example:
(check-expect (expunge my-flat-employers "Ash" "Aipom")
'(("Brock" "Steelix" 3) ("Brock" "Geodude" 2)
("Misty" "Staryu" 3) ("Misty" "Psyduck" 2) ("Misty" "Pikachu" 3)))

(define (expunge lofr rankg-id rankd-id)
  (filter (lambda (a) (not (or (string=? rankg-id (first a))
                               (string=? rankd-id (second a))))) lofr))

;; tests:
(check-expect (expunge my-flat-employers "a" "b") my-flat-employers)
(check-expect (expunge my-flat-employers "Ash" "Ketchum")
'(("Brock" "Steelix" 3) ("Brock" "Geodude" 2)
("Brock" "Aipom" 1) ("Misty" "Staryu" 3)
("Misty" "Psyduck" 2) ("Misty" "Pikachu" 3)
("Dawn" "Aipom" 1)))
(check-expect (expunge '() "a" "b") '())
(check-expect (expunge my-flat-employers "Ketchum" "Aipom")
'(("Ash" "Pikachu" 3) ("Ash" "Charizard" 2)
("Ash" "Geodude" 1) ("Brock" "Steelix" 3) ("Brock" "Geodude" 2)
("Misty" "Staryu" 3) ("Misty" "Psyduck" 2) ("Misty" "Pikachu" 3)))
(check-expect (expunge my-flat-employers "Aipom" "Ash")
              my-flat-employers)

;; (ranking-sort lofr) alphabetizes a list of FlatRankings
;; ranking-sort: (listof FlatRanking) -> (listof FlatRanking)
;; example:
(check-expect (ranking-sort
'(("a" "c" 1) ("z" "b" 1) ("a" "a" 1)))
'(("a" "a" 1) ("a" "c" 1) ("z" "b" 1)))

(define (ranking-sort lofr)
  (sort lofr (lambda (a b)
               (or (string<? (first a) (first b))
                   (and (string=? (first a) (first b))
                        (string<? (second a) (second b)))))))

;; tests:
(check-expect (ranking-sort my-flat-employers)
'(("Ash" "Aipom" 2) ("Ash" "Charizard" 2)
("Ash" "Geodude" 1) ("Ash" "Pikachu" 3)
("Brock" "Aipom" 1) ("Brock" "Geodude" 2)
("Brock" "Steelix" 3) ("Dawn" "Aipom" 1) 
("Misty" "Pikachu" 3) ("Misty" "Psyduck" 2) 
("Misty" "Staryu" 3)))
(check-expect (ranking-sort '()) '())
(check-expect (ranking-sort
'(("Ash" "Aipom" 2) ("Ash" "Charizard" 2)
("Ash" "Geodude" 1) ("Ash" "Pikachu" 3)
("Brock" "Aipom" 1) ("Brock" "Geodude" 2)
("Brock" "Steelix" 3) ("Dawn" "Aipom" 1) 
("Misty" "Pikachu" 3) ("Misty" "Psyduck" 2) 
("Misty" "Staryu" 3)))
'(("Ash" "Aipom" 2) ("Ash" "Charizard" 2)
("Ash" "Geodude" 1) ("Ash" "Pikachu" 3)
("Brock" "Aipom" 1) ("Brock" "Geodude" 2)
("Brock" "Steelix" 3) ("Dawn" "Aipom" 1) 
("Misty" "Pikachu" 3) ("Misty" "Psyduck" 2) 
("Misty" "Staryu" 3)))

;; (unfold lor) takes a list of Rankings and produces a list of FlatRankings
;; unfold: (listof Ranking) -> (listof FlatRanking)
;; example:
(check-expect (unfold my-employers) my-flat-employers)

(define (unfold lor)
  (foldr (lambda (a b)
           (append (foldr (lambda (c d)
                            (cons (cons (first a) c) d))
                          empty (second a))
                   b)) empty lor))

;; tests:
(check-expect (unfold '()) '())
(check-expect (unfold '(("Pikachu" (("Ash" 3) ("Misty" 3)))))
                      '(("Pikachu" "Ash" 3)
                        ("Pikachu" "Misty" 3)))

;; (find key lol) finds the first element in a list of lists that contains
;; the key or alse is the key isn't there
;; find: Any (listof (listof Any)) -> (anyof (listof Any) Bool)
;; example:
(check-expect (find "Ash" my-flat-employers)
              '("Ash" "Pikachu" 3))

(define (find key lol)
  (foldr (lambda (a b) (cond [(equal? (first a) key) a]
                             [else b])) false lol))

;; tests:
(check-expect (find "Aipom" my-flat-employers) false)
(check-expect (find "Dawn" my-flat-employers) '("Dawn" "Aipom" 1))
(check-expect (find "a" '()) false)
(check-expect (find "a" my-flat-employers) false)

;; (find-pref lor rankg-id rankd-id) produces the preference value for a pair of
;; rankg-id and rankd-id from a Ranking list
;; find-ref: (listof Ranking) Ranking Ranking -> Num
;; requires: the ranking with the rankg-id and rand-id must exist in the
;; Ranking list
;; example:
(check-expect (find-pref my-employers "Ash" "Pikachu") 3)

(define (find-pref lor rankg-id rankd-id)
  (second (first (filter (lambda (a) (string=? (first a) rankd-id))
                  (second (first
                           (filter (lambda (b) (string=? (first b) rankg-id))
                                   lor)))))))

;; tests:
(check-expect (find-pref my-employers "Dawn" "Aipom") 1)
(check-expect (find-pref my-students "Pikachu" "Ash") 3)
(check-expect (find-pref my-employers "Brock" "Geodude") 2)
(check-expect (find-pref my-employers "Brock" "Steelix") 3)
(check-expect (find-pref my-students "Charizard" "Ash") 2)
