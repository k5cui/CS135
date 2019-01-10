;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tables) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 07, Problem 4
;; ************************************************************************
;;

;; A Table is a (listof (listof Any))
;; requires: Every sublist of the table has the same length

(define mixed-table
  (list (list 3 (make-posn 4 2) "hello" false 4 -3)
        (list 3.2 #\q #\r "baby" -4.2 -6)
        (list '(1 2 3) 4 true false "quack" -9)
        (list "what" "is" "this" "even?" "Oy!" "minus 12")))

;; (transpose table) switches the rows and columns of a table
;; transpose: Table -> Table
;; example:
(check-expect (transpose mixed-table)
              (list (list 3 3.2 '(1 2 3) "what")
                    (list (make-posn 4 2) #\q 4 "is")
                    (list "hello" #\r true "this")
                    (list false "baby" false "even?")
                    (list 4 -4.2 "quack" "Oy!")
                    (list -3 -6 -9 "minus 12")))

(define (transpose table)
  (local [;; (getval val row) gets the value of a list at index val
          ;; getval: Nat (listof Any) -> Any
          ;; requires: val <= (length row)
          
          (define (getval val row)
            (cond [(= 0 val) (first row)]
                  [else (getval (sub1 val) (rest row))]))
          
          ;; (getcol col table-two) produces a column of a 2D list
          ;; getcol: Nat (listof (listof Any)) -> (listof Any)
          ;; requires: col <= (length row)
          
          (define (getcol col table-two)
            (cond [(empty? table-two) empty]
                  [else (cons (getval col (first table-two))
                              (getcol col (rest table-two)))]))

          ;; (gettable val table-three) switches the rows and columns of a table
          ;; gettable: Nat Table -> Table

          (define (gettable val table-three)
            (cond [(= val (length (first table-three))) empty]
                  [else (cons (getcol val table-three)
                              (gettable (add1 val) table-three))]))]
    (cond [(empty? table) empty]
          [else (gettable 0 table)])))

;; tests:
(check-expect (transpose (list (list 1 2 3)))
              (list (list 1)
                    (list 2)
                    (list 3)))
(check-expect (transpose (list (list 1)
                               (list 2)
                               (list 3)))
              (list (list 1 2 3)))
(check-expect (transpose empty)
              empty)

(check-expect (transpose (list empty empty empty)) empty)

