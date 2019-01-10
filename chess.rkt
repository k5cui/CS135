;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 03, Problem 3
;; ************************************************************************
;;

(define-struct square (row column))
;; A Square is a (make-square Nat Sym)
;; requires: 1 <= row <= 8
;; column: (anyof 'a 'b 'c 'd 'e 'f 'g 'h)

(define-struct piece (pos unit))
;; A Piece is a (make-piece square Sym)
;; requires: unit: (anyof ’knight ’rook ’bishop ’queen)

;; my-piece-fn: piece -> Any
(define (my-piece-fn piece)
  (... (piece-pos info)...
       (piece-unit info)...))

;; (column-number column) changes the number value of a column into a symbol
;; column-number: Sym -> Num
;; requires: column be (anyof 'a 'b 'c 'd 'e 'f 'g 'h)
;; example:
(check-expect (column-number 'c) 3)

(define (column-number column)
  (cond [(symbol=? column 'a) 1]
        [(symbol=? column 'b) 2]
        [(symbol=? column 'c) 3]
        [(symbol=? column 'd) 4]
        [(symbol=? column 'e) 5]
        [(symbol=? column 'f) 6]
        [(symbol=? column 'g) 7]
        [(symbol=? column 'h) 8]))

;; (number-column n) changes the symbols value of a column into a number
;; number-column: Num -> Sym
;; requires: n be > 0 and <= 8
;; example:
(check-expect (number-column 3) 'c)

(define (number-column n)
  (cond [(= n 1) 'a]
        [(= n 2) 'b]
        [(= n 3) 'c]
        [(= n 4) 'd]
        [(= n 5) 'e]
        [(= n 6) 'f]
        [(= n 7) 'g]
        [(= n 8) 'h]))

;; tests:
(check-expect (number-column 1) 'a)
(check-expect (number-column 2) 'b)
(check-expect (number-column 4) 'd)
(check-expect (number-column 5) 'e)
(check-expect (number-column 6) 'f)
(check-expect (number-column 7) 'g)
(check-expect (number-column 8) 'h)

;; (find-row find-my-row) produces the row of a chess piece or a square
;; find-row: anyof(Piece, Square) -> Num
;; example:
(check-expect (find-row (make-square 3 'f)) 3)

(define (find-row find-my-row)  
  (cond [(piece? find-my-row)
         (square-row (piece-pos find-my-row))]
        [(square? find-my-row)
         (square-row find-my-row)]))

;; (find-column find-my-column) produces the column of a chess piece or a square
;; find-row: anyof(Piece, Square) -> Num
;; example:
(check-expect (find-column (make-piece (make-square 5 'c) 'knight)) 3)

(define (find-column find-my-column)
  (cond [(piece? find-my-column)
         (column-number (square-column (piece-pos find-my-column)))]
        [(square? find-my-column)
         (column-number (square-column find-my-column))]))

;; (valid-move?) determines whether or not a chess piece can move to a
;; place on the board
;; valid-move?: Piece Square -> Bool
;; example:
(check-expect (valid-move? (make-piece (make-square 1 'a) 'knight)
                           (make-square 7 'b)) false)

(define (valid-move? chess-piece place)
  (cond
;; if the piece doesn't move
        [(and (= (find-row chess-piece) (find-row place))
              (= (find-column chess-piece) (find-column place))) true]
;; if the piece is a knight
        [(symbol=? (piece-unit chess-piece) 'knight)
         (cond [(and (< (abs (- (find-row chess-piece)
                                (find-row place))) 3)
                     (< (abs (- (find-column chess-piece)
                                (find-column place))) 3))
                (= (+ (abs (- (find-row chess-piece)
                           (find-row place)))
                      (abs (- (find-column chess-piece)
                              (find-column place)))) 3)]
               [else false])]
;; if the piece is a rook
        [(symbol=? (piece-unit chess-piece) 'rook)
         (or (= (find-row chess-piece) (find-row place))
             (= (find-column chess-piece) (find-column place)))]
;; if the piece is a bishop
        [(symbol=? (piece-unit chess-piece) 'bishop)
         (= (abs (- (find-row chess-piece) (find-row place)))
            (abs (- (find-column chess-piece) (find-column place))))]
;; if the piece is a queen
        [(symbol=? (piece-unit chess-piece) 'queen)
         (or (= (find-row chess-piece) (find-row place))
             (= (find-column chess-piece) (find-column place))
             (= (abs (- (find-row chess-piece) (find-row place)))
                (abs (- (find-column chess-piece) (find-column place)))))]))

;; tests:
(check-expect (valid-move? (make-piece (make-square 2 'a) 'knight)
                           (make-square 2 'a)) true)
(check-expect (valid-move? (make-piece (make-square 3 'c) 'knight)
                           (make-square 5 'd)) true)
(check-expect (valid-move? (make-piece (make-square 4 'd) 'knight)
                           (make-square 5 'f)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'knight)
                           (make-square 2 'h)) true)
(check-expect (valid-move? (make-piece (make-square 3 'a) 'knight)
                           (make-square 1 'b)) true)
(check-expect (valid-move? (make-piece (make-square 7 'g) 'knight)
                           (make-square 5 'f)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 3 'c)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 5 'c)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 6 'd)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'rook)
                           (make-square 4 'h)) false)
(check-expect (valid-move? (make-piece (make-square 4 'f) 'rook)
                           (make-square 4 'h)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'rook)
                           (make-square 5 'f)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'rook)
                           (make-square 3 'a)) true)
(check-expect (valid-move? (make-piece (make-square 3 'g) 'rook)
                           (make-square 1 'g)) true)
(check-expect (valid-move? (make-piece (make-square 3 'g) 'bishop)
                           (make-square 1 'g)) false)
(check-expect (valid-move? (make-piece (make-square 3 'g) 'bishop)
                           (make-square 1 'e)) true)
(check-expect (valid-move? (make-piece (make-square 8 'a) 'bishop)
                           (make-square 1 'h)) true)
(check-expect (valid-move? (make-piece (make-square 6 'd) 'bishop)
                           (make-square 7 'c)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 7 'b)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 7 'c)) false)
(check-expect (valid-move? (make-piece (make-square 4 'f) 'queen)
                           (make-square 4 'h)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'queen)
                           (make-square 5 'f)) true)
(check-expect (valid-move? (make-piece (make-square 3 'f) 'queen)
                           (make-square 3 'a)) true)
(check-expect (valid-move? (make-piece (make-square 3 'g) 'queen)
                           (make-square 1 'g)) true)
(check-expect (valid-move? (make-piece (make-square 3 'g) 'queen)
                           (make-square 1 'e)) true)
(check-expect (valid-move? (make-piece (make-square 8 'a) 'queen)
                           (make-square 1 'h)) true)
(check-expect (valid-move? (make-piece (make-square 6 'd) 'queen)
                           (make-square 7 'c)) true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 7 'b)) true)

;; (knight-next-move knight) determines the best possible move for the knight
;; to be as close to square H1 as poosible, while having the lowest possible
;; row value
;; knight-next-move: Piece -> Square
;; requires: The piece must be must have unit 'knight
;; example:
(check-expect (knight-next-move (make-piece (make-square 4 'e) 'knight))
              (make-square 2 'f))

(define (knight-next-move knight)
  (cond [(and (< (find-row knight) 3) (> (find-column knight) 6))
         (make-square (find-row knight) (number-column (find-column knight)))]
        
        [(= (find-row knight) 1)
         (make-square (+ (find-row knight) 1)
                      (number-column (+ (find-column knight) 2)))]
        
        [(= (find-row knight) 2)
         (make-square (- (find-row knight) 1)
                      (number-column (+ (find-column knight) 2)))]
         
        [(= (find-column knight) 8)
         (make-square (- (find-row knight) 2)
                      (number-column (- (find-column knight) 1)))]

        [else (make-square (- (find-row knight) 2)
                           (number-column (+ (find-column knight) 1)))]))
                                                    
;; tests:
(check-expect (knight-next-move (make-piece (make-square 1 'g) 'knight))
              (make-square 1 'g))
(check-expect (knight-next-move (make-piece (make-square 1 'c) 'knight))
              (make-square 2 'e))
(check-expect (knight-next-move (make-piece (make-square 2 'c) 'knight))
              (make-square 1 'e))
(check-expect (knight-next-move (make-piece (make-square 7 'h) 'knight))
              (make-square 5 'g))
