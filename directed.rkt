;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 09, Problem 1
;; ************************************************************************
;;

;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))

;; 
(define a-graph
'((A (B C K)) (B (D E)) (C (I F)) (D (G J))
(E (H I)) (F ()) (H ()) (I ()) (K ())))

;; (find-node node graph) finds a route from an origin node to a destination
;; node in a given graph.
;; find-node: Node Lst-of-Graph -> Lst-of-Path

(define (find-node node graph)
  (cond [(empty? graph) false]
        [(symbol=? node (first (first graph)))
         (first graph)]
        [else (find-node node (rest graph))]))

;; (valid-route? path graph) consumes a path and graph and produces whether
;; or not the path is a valid route to the directed graph.
;; valid-route?: Lst-of-Path Lst-of-Graph -> Bool
;; examples:
(check-expect (valid-route? '(A C F) a-graph) true)
(check-expect (valid-route? '(A C I K) a-graph) false)
(check-expect (valid-route? '(D G) empty) false)

(define (valid-route? path graph)
  (cond [(empty? path) true]
        [(boolean? (find-node (first path) graph)) false]
        [(empty? (rest path)) true]
        [(member? (second path) (second (find-node (first path) graph)))
         (valid-route? (rest path) graph)]
        [else false]))

;; tests:
(check-expect (valid-route? '(A B E H) a-graph) true)
(check-expect (valid-route? '(B D G I) a-graph) false)
(check-expect (valid-route? '(A K) empty) false)
(check-expect (valid-route? '(A K) a-graph) true)
(check-expect (valid-route? empty a-graph) true)
(check-expect (valid-route? empty empty) true)
(check-expect (valid-route? '(X Y Z) a-graph) false)