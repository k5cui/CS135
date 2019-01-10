;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 08, Problem 3
;; ************************************************************************
;;

;; (partition predicate lst) takes a list and a predicate and produces a list of
;; 2 lists, the first containing values where the predicate was true and the
;; second where the predicate was false
;; partition: (Any -> Bool) (listof Any) -> (listof (listof Any) (listof Any))
;; example:
(check-expect (partition even? '(1 2 3 4 5)) '((2 4) (1 3 5)))

(define (partition predicate lst)
  (local [;; (part-true/acc predicate lst acc) accumulates a list of values for
          ;; which the predicate holds
          ;; part-true/acc:
          ;; (Any -> Bool) (listof Any) (listof Any) -> (listof Any)
          
          (define (part-true/acc predicate lst acc)
            (cond [(empty? lst) acc]
                  [(predicate (first lst))
                   (part-true/acc predicate (rest lst) (cons (first lst) acc))]
                  [else (part-true/acc predicate (rest lst) acc)]))
          
          ;; (part-false/acc predicate lst acc) accumulates a list of values for
          ;; which the predicate does not hold
          ;; part-false/acc:
          ;; (Any -> Bool) (listof Any) (listof Any) -> (listof Any)
          
          (define (part-false/acc predicate lst acc)
            (cond [(empty? lst) acc]
                  [(not (predicate (first lst)))
                   (part-false/acc predicate (rest lst) (cons (first lst) acc))]
                  [else (part-false/acc predicate (rest lst) acc)]))]
    
          (list (reverse (part-true/acc predicate lst empty))
                (reverse (part-false/acc predicate lst empty)))))

;; tests:
(check-expect (partition string? empty) '(() ()))
(check-expect (partition string? '(1 "3" 2 1)) '(("3") (1 2 1)))
(check-expect (partition number? '(apple orange pear))
              '(() (apple orange pear)))
(check-expect (partition number? '(1 2 3)) '((1 2 3) ()))
