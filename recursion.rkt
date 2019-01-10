;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (on-line lop) will produce true if all psns in the list
;; are on the line formed by y=3x+1, false otherise.
;; on-line: (listof Posn( -> Bool
(check-expect (on-line (cons (make-posn 5 16) (cons
                             (make-posn -1 -2) (cons
                             (make-posn 0 1) empty)))) true)

(check-expect (on-line (cons (make-posn 0 1) (cons
                             (make-posn 3 9) empty))) false)

(define (on-line lop)
  (cond [(empty? lop) true]
        [else (and (= (+ 1 (* (posn-x (first lop)) 3)) (posn-y (first lop)))
                   (on-line (rest lop)))]))
                                              