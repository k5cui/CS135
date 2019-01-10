;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rainbow2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 4
;; ************************************************************************
;;

;; (iscolour? value) determines if a given value is a colour of the rainbow
;; iscolour?: Any -> Bool
;; example:
(check-expect (iscolour? 'red) true)

(define (iscolour? value)
  (cond [(symbol? value)
         (or (symbol=? value 'red)
             (symbol=? value 'orange) 
             (symbol=? value 'yellow)
             (symbol=? value 'green)
             (symbol=? value 'blue)
             (symbol=? value 'indigo)
             (symbol=? value 'violet))]
        [else false]))

;; tests:
(check-expect (iscolour? 2) false)

;; (colournum colour) changes a colour of the rainbow to a numerical value
;; colournum: Sym -> Num
;; requires: (iscolour? colour) must be true
;; example:
(check-expect (colournum 'red) 1)

(define (colournum colour)
  (cond [(symbol=? colour 'red) 1]
        [(symbol=? colour 'orange) 2]
        [(symbol=? colour 'yellow) 3]
        [(symbol=? colour 'green) 4]
        [(symbol=? colour 'blue) 5]
        [(symbol=? colour 'indigo) 6]
        [(symbol=? colour 'violet) 7]))

;; (rainbow? list) determines if a given list of colours is a rainbow
;; rainbow?: (listof Any) -> Bool
;; example:
(check-expect (rainbow? (cons 'orange (cons 'green (cons 'blue empty)))) true)

(define (rainbow? list)
  (cond [(empty? list) true]
        [(iscolour? (first list))
         (cond [(empty? (rest list)) true]
               [(< (colournum (first list)) (colournum (first (rest list))))
                (rainbow? (rest list))]
               [else false])]
        [else false]))

;; tests:
(check-expect (rainbow? empty) true)
(check-expect (rainbow? (cons 'yellow empty)) true)
(check-expect (rainbow? (cons 'violet (cons 'green (cons 'blue empty)))) false)
(check-expect (rainbow? (cons 2 (cons 'indigo empty))) false)
(check-expect (rainbow? (cons 'red (cons 'indigo (cons 'indigo empty)))) false)

;; (unicorn colour rainbow) produces a new rainbow with a colour missing from it
;; unicorn: Sym (listof Sym) -> (listof Sym)
;; requires: (rainbow? rainbow) must be true
;;           (iscolour? colour) must be true
;; example:
(check-expect (unicorn 'red (cons 'red (cons 'green (cons 'indigo empty))))
              (cons 'green (cons 'indigo empty)))

(define (unicorn colour rainbow)
  (cond [(empty? rainbow) empty]
        [else (cond [(symbol=? colour (first rainbow))
                     (unicorn colour (rest rainbow))]
                    [else (cons (first rainbow)
                                (unicorn colour (rest rainbow)))])]))

;; tests:
(check-expect (unicorn 'red (cons 'yellow (cons 'green (cons 'indigo empty))))
              (cons 'yellow (cons 'green (cons 'indigo empty))))
(check-expect (unicorn 'red empty) empty)
(check-expect (unicorn 'green (cons 'red (cons 'green (cons 'indigo empty))))
              (cons 'red (cons 'indigo empty)))

;; (leprechaun colour rainbow) produces a new rainbow with a colour inserted
;; into it
;; leprechaun: Sym (listof Sym) -> (listof Sym)
;; requires: (rainbow? rainbow) must be true
;;           (iscolour? colour) must be true
;; example:
(check-expect
 (leprechaun 'red (cons 'yellow (cons 'green (cons 'indigo empty))))
 (cons 'red (cons 'yellow (cons 'green (cons 'indigo empty)))))

(define (leprechaun colour rainbow)
  (cond [(empty? rainbow) (cons colour empty)]
        [(empty? (rest rainbow)) (cons (first rainbow) (cons colour empty))]    
        [else (cond [(< (colournum colour) (colournum (first rainbow)))
                     (cons colour rainbow)]
                    [(= (colournum colour) (colournum (first rainbow)))
                     rainbow]
                    [(> (colournum colour) (colournum (first rainbow)))
                     (cons (first rainbow)
                           (leprechaun colour (rest rainbow)))])]))
(check-expect
 (leprechaun 'violet (cons 'red (cons 'yellow (cons 'indigo empty))))
 (cons 'red (cons 'yellow (cons 'indigo (cons 'violet empty)))))
(check-expect
 (leprechaun 'orange (cons 'orange (cons 'yellow (cons 'blue empty))))
 (cons 'orange (cons 'yellow (cons 'blue empty))))
(check-expect
 (leprechaun 'yellow empty)
 (cons 'yellow empty))
