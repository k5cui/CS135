;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname paint-by-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 1
;; ************************************************************************
;;

;; A Grid is a (listof (listof (anyof 'B '-)))
;; requires: all sublists have the same length

(define fish-grid
  '((B B - - B B B B - - - B)
    (- B B B - - - - B - - -)
    (- B B B - - - - B B - -)
    (- B - - B B B B B - - -)
    (B B - - - - B - - - - -)))

;; (getcol col row) produces the value of a list at index col
;; getcol: Nat (listof Any) -> Any
(define (getcol col list)
  (cond [(= 0 col) (first list)]
        [else (getcol (sub1 col) (rest list))]))

;; (column col grid) produces the column at column number col of a given grid
;; column: Nat Grid -> (listof (anyof 'B '-))
;; Examples:
(check-expect (column 0 fish-grid) '(B - - - B))

(define (column col grid)
  (cond [(empty? grid) empty]
        [(>= col (length (first grid))) empty]
        [else (cons (getcol col (first grid)) (column col (rest grid)))]))

;; Tests:
(check-expect (column 1 fish-grid) '(B B B B B))
(check-expect (column 1 empty) empty)
(check-expect (column 2 fish-grid) '(- B B - - ))
(check-expect (column 32 fish-grid) empty)

;; (b-row list) determines the amount of times 'B appears in a row starting from
;; the beginning of a list
;; b-row: (listof (anyof 'B '-)) -> Nat
;; Examples:
(check-expect (b-row '(B B B - -)) 3)

(define (b-row list)
  (cond [(or (empty? list) (symbol=? (first list) '-)) 0]
        [else (add1 (b-row (rest list)))]))

;; (short-list n list) cuts off the first n terms of a list
;; short-list: Nat (listof Any) -> (listof Any)
;; Examples:
(check-expect (short-list 3 '(1 2 3 4 5))
              '(4 5))

(define (short-list n list)
  (cond [(= 0 n) list]
        [else (short-list (sub1 n) (rest list))]))

;; (cells->tallies list) produces a list of tallies of adjacent black cells
;;   for the given list, where each tally is represented by a Nat
;; cells->tallies: (listof (anyof 'B '-)) -> (listof Nat)
;; Examples:
(check-expect (cells->tallies (first fish-grid)) (list 2 4 1))

(define (cells->tallies list)
  (cond [(empty? list) empty]
        [(symbol=? 'B (first list))
         (cons (b-row list) (cells->tallies (short-list (b-row list) list)))]
        [else (cells->tallies (rest list))]))

;; Tests:
(check-expect (cells->tallies empty) empty)
(check-expect (cells->tallies '(B)) (list 1))
(check-expect (cells->tallies '(-)) empty)
(check-expect (cells->tallies (second fish-grid)) (list 3 1))

;; (column-labels grid n) produces labels for all the columns of a grid
;; starting from index n
;; column-labels: Grid Nat -> (listof (listof Nat))
;; Examples:
(check-expect (column-labels fish-grid 0)
              (list (list 1 1) (list 5) (list 2)
                    (list 2) (list 1 1) (list 1 1)
                    (list 1 2) (list 1 1) (list 3)
                    (list 1) empty (list 1)))

(define (column-labels grid n)
  (cond [(empty? grid) empty]
        [(= n (length (first grid))) empty]
        [else (cons (cells->tallies (column n grid))
                    (column-labels grid (add1 n)))]))

;; (row-labels grid) produces labels for all the rows of a grid
;; row-labels: Grid -> (listof (listof Nat))
;; Examples:
(check-expect (row-labels fish-grid)
              (list (list 2 4 1) (list 3 1) (list 3 2)
                    (list 1 5) (list 2 1)))
      
(define (row-labels grid)
  (cond [(empty? grid) empty]
        [else (cons (cells->tallies (first grid))
                    (row-labels (rest grid)))]))

;; (puzzle-labels grid) produces labels for the given grid. Each label is
;; represented by a list of natural numbers, corresponding to the
;; adjacent black squares in that row or column
;; puzzle-labels: Grid -> (listof (listof (listof Nat)))
;; Examples:
(check-expect (puzzle-labels fish-grid)
(list (list (list 2 4 1) (list 3 1) (list 3 2)
            (list 1 5) (list 2 1))
      (list (list 1 1) (list 5) (list 2)
            (list 2) (list 1 1) (list 1 1)
            (list 1 2) (list 1 1) (list 3)
            (list 1) empty (list 1))))

(define (puzzle-labels grid)
  (cons (row-labels grid)
        (cons (column-labels grid 0) empty)))

;; Tests:
(check-expect (puzzle-labels empty)
              (list empty empty))
(check-expect (puzzle-labels (list (list '-)))
              (list (list empty) (list empty)))
(check-expect (puzzle-labels (list (list 'B)))
              (list (list (list 1)) (list (list 1))))