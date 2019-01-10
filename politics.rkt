;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname politics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 3
;; ************************************************************************

;; A Perk is a (list Nat Str)
;; requires: Nat > 0

;; A Perk-list is either
;; * empty
;; * (cons (list Str (listof Perk)) Perk-list)
;; requires: Perks in (listof Perk) sorted by compliancy score
;; (non-increasing)

(define short-perklist
  (list
   (list "Peter Smith"
         (list
          (list 83 "50,000 dollars campaign funding")
          (list 32 "Public support by your company")
          (list 13 "Opera tickets")))
   (list "Jennifer O'Brien"
         (list
         (list 137 "Position on the Board of Directors")
         (list 22 "Arranging photo-op with CEO")))
   (list "Steve Li"
         (list
          (list 91 "Sponsored TV ads")
          (list 56 "Invitation as keynote-speaker")
          (list 9 "Positive press release in his favour")
          (list 5 "Business dinner with CTO")))))

;; (combine-perks perks1 perks2) combines the perks of a politician with the
;; same name
;; combine-perks: (listof Perk) (listof Perk) -> (listof Perk)
;; Examples:
(check-expect (combine-perks (list
                              (list 83 "50,000 dollars campaign funding")
                              (list 32 "Public support by your company")
                              (list 13 "Opera tickets"))
                             (list
                              (list 137 "Position on the Board of Directors")
                              (list 22 "Arranging photo-op with CEO")))  
              (list
              (list 137 "Position on the Board of Directors")
              (list 83 "50,000 dollars campaign funding")
              (list 32 "Public support by your company")
              (list 22 "Arranging photo-op with CEO")
              (list 13 "Opera tickets")))
                             

(define (combine-perks perks1 perks2)
  (cond [(empty? perks1) perks2]
        [(empty? perks2) perks1]
        [(>= (first (first perks2)) (first (first perks1)))
         (cons (first perks2) (combine-perks perks1 (rest perks2)))]
        [else (cons (first perks1) (combine-perks (rest perks1) perks2))]))

;; (add-perks name newperks perklist) produces a new perklist when given a list
;;   of newperks, given name and a pre-existing perklist
;; add-perks: Str (listof Perk) Perk-list -> Perk-list
;; Examples:
(check-expect (add-perks "Kenneth Cui" (list (list 10 "Cars")) short-perklist)
              (list
 (list "Peter Smith" (list (list 83 "50,000 dollars campaign funding")
                           (list 32 "Public support by your company")
                           (list 13 "Opera tickets")))
 (list "Jennifer O'Brien" (list (list 137 "Position on the Board of Directors")
                                (list 22 "Arranging photo-op with CEO")))
 (list
  "Steve Li"
  (list
   (list 91 "Sponsored TV ads")
   (list 56 "Invitation as keynote-speaker")
   (list 9 "Positive press release in his favour")
   (list 5 "Business dinner with CTO")))
 (list "Kenneth Cui" (list (list 10 "Cars")))))

(define (add-perks name newperks perklist)
  (cond [(empty? perklist) (cons (list name newperks) empty)]
        [(string=? (first (first perklist)) name)
         (cons (list name (combine-perks (first (rest (first perklist)))
                                         newperks))
               (rest perklist))]
        [else (cons (first perklist)
                    (add-perks name newperks (rest perklist)))]))

;; Tests:
(check-expect (add-perks "Jennifer O'Brien"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour"))
                         short-perklist)
              (list
               (list "Peter Smith"
                     (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 32 "Public support by your company")
                      (list 13 "Opera tickets")))
               (list "Jennifer O'Brien"
                     (list
                      (list 137 "Position on the Board of Directors")
                      (list 30 "Two free flights in company jet")
                      (list 22 "Arranging photo-op with CEO")
                      (list 3 "Guided company tour")))
               (list "Steve Li"
                     (list
                      (list 91 "Sponsored TV ads")
                      (list 56 "Invitation as keynote-speaker")
                      (list 9 "Positive press release in his favour")
                      (list 5 "Business dinner with CTO")))))

;; (best-perk score perk-set) determines the best perk an individual can obtain
;; given a score and a list of perks
;; best-perk: Int (listof Str (listof Perk)) -> Str
;; requires: score >= 0
;;           perk-set cannot be empty 
;; Examples:
(check-expect (best-perk 33 (first (rest (first short-perklist))))
              "Public support by your company")

(define (best-perk score perk-set)
  (cond [(> score (first (first perk-set)))
         (first (rest (first perk-set)))]
        [else (best-perk score (rest perk-set))]))

;; (perk-received name score perklist) produces a given perk based on the given
;; name of the politician, their score and their perk-list
;; perk-received: Str Int Perk-list -> (anyof Str Sym)
;; Examples:
(check-expect (perk-received "Jennifer O'Brien" -9 short-perklist)
              'smear-campaign)
(check-expect (perk-received "Steve Li" 92 short-perklist) "Sponsored TV ads")

(define (perk-received name score perklist)
  (cond [(< score 0) 'smear-campaign]
        [(empty? perklist) 'wristwatch]
        [(string=? name (first (first perklist)))
         (best-perk score (first (rest (first perklist))))]
        [else (perk-received name score (rest perklist))]))

;; Tests:
(check-expect (perk-received "Jennifer O'Brien" 25 short-perklist)
              "Arranging photo-op with CEO")
(check-expect (perk-received "Noton Thelist" 43 short-perklist)
              'wristwatch)
(check-expect (perk-received "Steve Li" 12 short-perklist)
              "Positive press release in his favour")
(check-expect (perk-received "Peter Smith" -25 short-perklist)
              'smear-campaign)

;; An Action is a (list Str Int Str)
(define-struct actionnode (name score actions left right))
;; An ActionNode is a (make-actionnode Str Int (listof Action)
;; ActionSearchTree ActionSearchTree)
;; requires:
;; (string<? x name) is true for every (actionnode-name x)
;; in the left subtree
;; (string>? x name) is true for every (actionnode-name x)
;; in the right subtree

;; An ActionSearchTree is one of:
;; * empty
;; * an ActionNode

(define short-ast (make-actionnode "Amanda Byers" -5 (list
(list "Amanda Byers" -5 "Met with competitor"))
empty empty));
(check-expect
(add-action (list "Amanda Byers" -5 "Met with competitor") empty)
short-ast)
(check-expect
(add-action (list "Amanda Byers" 7 "Argued on talk radio against
raising minimum wage") short-ast)
(make-actionnode "Amanda Byers" 2 (list
(list "Amanda Byers" 7 "Argued on talk radio against
raising minimum wage")
(list "Amanda Byers" -5 "Met with competitor"))
empty empty))

;; (add-action action action-tree) produce a new action tree with a given action
;;   added onto the original action-tree
;; add-action: Action ActionSearchTree -> ActionSearchTree
;; Examples:
(check-expect (add-action (list "Kenneth Cui" 100
                                "Finished Assignment") short-ast)
              (make-actionnode "Amanda Byers" -5
                               (list
                                (list "Amanda Byers" -5 "Met with competitor"))
                               '()
                               (make-actionnode
                                "Kenneth Cui" 100
                                (list (list "Kenneth Cui" 100
                                            "Finished Assignment")) '() '())))

(define (add-action action action-tree)
  (cond [(empty? action-tree)
         (make-actionnode (first action)
                          (second action)
                          (list action) empty empty)]
        [(string=? (first action) (actionnode-name action-tree))
         (make-actionnode (first action)
                          (+ (actionnode-score action-tree) (second action))
                          (append (list action)
                                  (actionnode-actions action-tree))
                          (actionnode-left action-tree)
                          (actionnode-right action-tree))]
        [(string>? (first action) (actionnode-name action-tree))
         (make-actionnode (actionnode-name action-tree)
                          (actionnode-score action-tree)
                          (actionnode-actions action-tree)
                          (actionnode-left action-tree)
                          (add-action action (actionnode-right action-tree)))]
        [(string<? (first action) (actionnode-name action-tree))
         (make-actionnode (actionnode-name action-tree)
                          (actionnode-score action-tree)
                          (actionnode-actions action-tree)
                          (add-action action (actionnode-left action-tree))
                          (actionnode-right action-tree))]))

;; Tests:
(check-expect (add-action
               (list "A" 50 "pizza")
(make-actionnode "Amanda Byers" -5
(list
(list "Amanda Byers" -5 "Met with competitor"))
empty
(make-actionnode "Steve Li" 12
(list
(list "Steve Li" 12 "Plays golf with your second cousin"))
(make-actionnode "Jennifer O'Brien" 25
(list
(list "Jennifer O'Brien" 30 "Pushed major contract for your
company")
(list "Jennifer O'Brien" 5 "Mentioned your company on
morning TV")
(list "Jennifer O'Brien" -10 "Questioned your leadership in
public"))
empty
empty)
empty)))
(make-actionnode "Amanda Byers" -5
(list
(list "Amanda Byers" -5 "Met with competitor"))
(make-actionnode "A" 50
(list
(list "A" 50 "pizza")) empty empty)
(make-actionnode "Steve Li" 12
(list
(list "Steve Li" 12 "Plays golf with your second cousin"))
(make-actionnode "Jennifer O'Brien" 25
(list
(list "Jennifer O'Brien" 30 "Pushed major contract for your
company")
(list "Jennifer O'Brien" 5 "Mentioned your company on
morning TV")
(list "Jennifer O'Brien" -10 "Questioned your leadership in
public"))
empty
empty)
empty)))

;; (perk-list action-tree Perk-list) produces a list of pairs. The first element
;;   of each pair is the politician's name, and the second is the perk received
;;   given a specific action-tree and Perk-list
;; perk-list: ActionSearchTree Perk-list ->(listof (listof Str (anyof Str Sym)))
;; Examples:
(check-expect (perk-list (make-actionnode "Jennifer O'Brien" 50
                               (list
                                (list "Jennifer O'Brien" 30 "Promoted Ads")
                                (list "Jennifer O'Brien" 20 "Handed Flyers"))
                               empty empty) short-perklist)
              (list (list "Jennifer O'Brien" "Arranging photo-op with CEO")))
(define (perk-list action-tree Perk-list)
  (cond [(empty? action-tree) empty]
        [else (append (perk-list (actionnode-left action-tree) Perk-list)
                      (list (list
                             (actionnode-name action-tree)
                             (perk-received (actionnode-name action-tree)
                                            (actionnode-score action-tree)
                                            Perk-list)))
                      (perk-list (actionnode-right action-tree) Perk-list))]))

;; Tests:
(check-expect (perk-list
(make-actionnode "Amanda Byers" -5
(list
(list "Amanda Byers" -5 "Met with competitor"))
empty
(make-actionnode "Steve Li" 12
(list
(list "Steve Li" 12 "Plays golf with your second cousin"))
(make-actionnode "Jennifer O'Brien" 25
(list
(list "Jennifer O'Brien" 30 "Pushed major contract for your
company")
(list "Jennifer O'Brien" 5 "Mentioned your company on
morning TV")
(list "Jennifer O'Brien" -10 "Questioned your leadership in
public"))
empty
empty)
empty))
(list
(list "Jennifer O'Brien" (list
(list 137 "Position on the Board of Directors")
(list 30 "Two free flights in company jet")
(list 22 "Arranging photo-op with CEO")
(list 3 "Guided company tour")))
(list "Steve Li" (list
(list 91 "Sponsored TV ads")
(list 56 "Invitation as keynote-speaker")
(list 9 "Positive press release in his favour")
(list 5 "Business dinner with CTO")))))
(list
(list "Amanda Byers" 'smear-campaign)
(list "Jennifer O'Brien" "Arranging photo-op with CEO")
(list "Steve Li" "Positive press release in his favour")))





          
