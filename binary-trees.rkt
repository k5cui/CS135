;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binary-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 2
;; ************************************************************************
;;

;; A Binary Search Tree (BST) is one of:
;; * empty
;; * a Node
(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;; key < every key in right BST

(define my-bst (make-node 5 (make-node 3 empty empty) (make-node 9
(make-node 7 empty empty) empty)))

(define another-bst (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 9 empty empty)))

;; (bst-count bst) produces the total number (as Nat) of nodes in a given
;;   bst (Binary Search Tree)
;; bst-count: BST -> Nat
;; Examples:
(check-expect (bst-count my-bst) 4)

(define (bst-count bst)
  (cond [(empty? bst) 0]
        [else (+ 1 (bst-count (node-right bst)) (bst-count (node-left bst)))]))

(check-expect (bst-count empty) 0)
(check-expect (bst-count (make-node 1 empty empty)) 1)

;; (bst-add key bst) produces a new bst with a new node added, given
;; a key and a pre-existing bst. THe new node contains the given key
;; bst-add: Nat BST -> BST
;; Examples:
(check-expect
(bst-add 4 my-bst)
(make-node 5 (make-node 3 empty (make-node 4 empty empty))
(make-node 9 (make-node 7 empty empty) empty)))

(define (bst-add key bst)
  (cond [(empty? bst)(make-node key empty empty)]
        [(> key (node-key bst))
         (make-node (node-key bst)
                    (node-left bst)
                    (bst-add key (node-right bst)))]
        [(< key (node-key bst))
         (make-node (node-key bst)
                    (bst-add key (node-left bst))
                    (node-right bst))]
        [else bst]))

;; Tests:
(check-expect (bst-add 5 empty) (make-node 5 empty empty))
(check-expect (bst-add 5 (make-node 5 empty empty)) (make-node 5 empty empty))
(check-expect (bst-add 1 my-bst) (make-node 5
 (make-node 3 (make-node 1 '() '()) '())
 (make-node 9 (make-node 7 '() '()) '())))

;; (bst-height bst) produces the height of a given bst, which is the
;;   maximum distance between the tree's root and it's leaves
;; bst-height: BST -> Nat
;; Examples:
(check-expect
(bst-height another-bst)
3)

(define (bst-height bst)
  (cond [(or (empty? bst)
             (and (empty? (node-right bst))
                  (empty? (node-left bst)))) 0]
        [else (+ 1 (max (bst-height (node-right bst))
                        (bst-height (node-left bst))))]))

;; Tests:
(check-expect (bst-height empty) 0)
(check-expect (bst-height (make-node 1 empty empty)) 0)

;; (longest-path bst) determines the longest path of a given bst
;; longest-path: BST -> Nat
;; Examples:
(check-expect (longest-path another-bst) 4)

(define (longest-path bst)
  (cond [(empty? bst) 0]
        [else (+ (bst-height bst) 1)]))

;; (bst-balanced? bst) determines if the given bst is balanced or not.
;;   A balanced tree is described in the assignment
;; bst-balanced?: BST -> Bool
;; Examples:
(check-expect
(bst-balanced? my-bst)
true)

(define (bst-balanced? bst)
  (cond [(empty? bst) true]
        [(and (<= (abs (- (longest-path (node-right bst))
                          (longest-path (node-left bst)))) 1)
              (bst-balanced? (node-right bst))
              (bst-balanced? (node-left bst)))
         true]
        [else false]))

;; Tests:
(check-expect
(bst-balanced? another-bst)
false)
(check-expect (bst-balanced? empty) true)
(check-expect (bst-balanced? (make-node 4 (make-node 1
                                      (make-node 0 empty empty)
                                      (make-node 2 empty empty))
                         (make-node 7 empty
                                    (make-node 9
                                               (make-node 8 empty empty)
                                               empty)))) false)

