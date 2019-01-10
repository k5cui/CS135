;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 4
;; ************************************************************************
;;

;; A doc-list (DL) is one of:
;; * empty
;; * (cons Str DL)
;; requires: each doc (i.e. Str) only occurs once in the doc-list
;; the doc-list is in ascending alphabetical order

;; An Inverted List (IL) is one of:
;; * empty
;; * (cons (list Str DL) IL)
;; requires: each key (i.e. Str) only occurs once in the IL.
;; the keys occur in ascending alphabetical order in the IL.

;; (doc-there? doc doclist) determines if a doc exists in a DL
;; doc-there?: Str DL -> Bool
;; example:
(check-expect (doc-there? "b.txt" (list "b.txt" "c.txt")) true)

(define (doc-there? doc doclist)
  (cond [(empty? doclist) false]
        [(string=? doc (first doclist))
         true]
        [else (doc-there? doc (rest doclist))]))

;; (both dl1 dl2): produces a list of documents
;; (possibly empty) that occur in both DLs.
;; both: DL DL -> (listof Str)
;; example:
(check-expect (both (list "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))

(define (both dl1 dl2)
  (cond [(empty? dl1) empty]
        [(doc-there? (first dl1) dl2)
         (cons (first dl1) (both (rest dl1) dl2))]
        [else (both (rest dl1) dl2)]))

;; tests:
(check-expect (both (list "b.txt" "c.txt") (list "b.txt" "c.txt"))
              (list "b.txt" "c.txt"))
(check-expect (both (list "b.txt" "c.txt" "z.txt") (list "b.txt" "t.txt"))
              (list "b.txt"))
(check-expect (both (list  "7.txt") (list "b.txt" "c.txt"))
              '())
(check-expect (both (list "b.txt" "c.txt") (list "c.txt"))
              (list "c.txt"))

;; (exclude dl1 dl2): produces a list of documents
;; (possibly empty) that occur in the first DL but not the second one.
;; exclude: Doc-list Doc-list -> Str
;; example:
(check-expect (exclude (list "7.txt") (list "b.txt" "c.txt"))
              (list "7.txt"))

(define (exclude dl1 dl2)
  (cond [(empty? dl1) empty]
        [(empty? (both dl1 dl2)) dl1]
        [(string=? (first dl1) (first (both dl1 dl2)))
         (exclude (rest dl1) (rest dl2))]
        [else (cons (first dl1) (exclude (rest dl1) dl2))]))

;; tests:
(check-expect (exclude (list "b.txt") (list "b.txt" "c.txt" "lol.txt"))
              '())
(check-expect (exclude (list "b.txt" "c.txt") (list "b.txt"))
              (list "c.txt"))
(check-expect (exclude (list "dududud.txt" "7.txt") (list "7.txt"))
              (list "dududud.txt"))

;; (find-list str IL) finds the DL for a given string from an Inverted List
;; find-list: Str IL -> DL
;; example:
(check-expect (find-list "grape"
(list (list "apple" (list "b.txt"))
(list "orange" (list "a.txt" "c.txt"))
(list "banana" (list "c.txt"))
(list "pear" (list "b.txt" "c.txt"))
(list "grape" (list "a.txt"))
(list "melon" (list "c.txt"))
(list "berry" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))

(define (find-list str IL)
  (cond [(empty? IL) empty]
        [(string=? str (first (first IL)))
         (first (rest (first IL)))]
        [else (find-list str (rest IL))]))

;; (search symb str1 str2 IL): produces produces a (possibly empty) list of
;; documents. The arguments for search will always be in one of two possible
;; formats:
;; - (search ’both str1 str2 IL) which produces a list of documents from
;; an-IL that contains both the words str1 and str2 and
;; - (search ’exclude str1 str2 IL) which produces a list of documents from
;; an-IL that contains the word str1 but not the word str2.
;; search: Sym Str Str IL -> DL
;; example:
(check-expect (search 'both "orange" "banana"
(list (list "apple" (list "b.txt"))
(list "orange" (list "a.txt" "c.txt"))
(list "banana" (list "c.txt"))
(list "pear" (list "b.txt" "c.txt"))
(list "grape" (list "a.txt"))
(list "melon" (list "c.txt"))
(list "berry" (list "a.txt" "b.txt" "c.txt")))) (list "c.txt"))

(define (search symb str1 str2 IL)
  (cond [(symbol=? symb 'both)
         (both (find-list str1 IL) (find-list str2 IL))]
        [(symbol=? symb 'exclude)
         (exclude (find-list str1 IL) (find-list str2 IL))]))

;; tests:
(check-expect (search 'both "apple" "banana"
(list (list "apple" (list "b.txt"))
(list "orange" (list "a.txt" "c.txt"))
(list "banana" (list "c.txt"))
(list "pear" (list "b.txt" "c.txt"))
(list "grape" (list "a.txt"))
(list "melon" (list "c.txt"))
(list "berry" (list "a.txt" "b.txt" "c.txt")))) '())

(check-expect (search 'exclude "orange" "banana"
(list (list "apple" (list "b.txt"))
(list "orange" (list "a.txt" "c.txt"))
(list "banana" (list "c.txt"))
(list "pear" (list "b.txt" "c.txt"))
(list "grape" (list "a.txt"))
(list "melon" (list "c.txt"))
(list "berry" (list "a.txt" "b.txt" "c.txt")))) (list "a.txt"))

(check-expect (search 'exclude "melon" "banana"
(list (list "apple" (list "b.txt"))
(list "orange" (list "a.txt" "c.txt"))
(list "banana" (list "c.txt"))
(list "pear" (list "b.txt" "c.txt"))
(list "grape" (list "a.txt"))
(list "melon" (list "c.txt"))
(list "berry" (list "a.txt" "b.txt" "c.txt")))) '())
