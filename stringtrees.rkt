;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname stringtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 07, Problem 3
;; ************************************************************************
;;

(define celebs '("Punk"
                 (("Tokido" ())
                  ("Marn" (("JWong" ()) ("Ricki" ()) ("KBrad" ())))
                  ("Smug" (("Fuudo" ())
                           ("Mena" (("Caba" ())
                                    ("NuckleDu" ())))
                           ("SnakeEyez" ()))))))

;; A StringTree is a (list Str (listof StringTree))

(define big-example-tree '("food" (("plant" (("fruit" (("apple" ())
                                                       ("banana" ())))
                                             ("veggie" ())))
                                   ("animal" (("dairy" (("milk"
                                                         (("cheese"
                                                           (("pizza" ())))))))
                                              ("meat" (("beef" ())
                                                       ("chicken" ())
                                                       ("pork" ())
                                                       ("lamb" ()))))))))

(define small-example-tree '("animal" (("dog" (("gold" (("retriever" ()))))))))

;; stringtree-template: StringTree -> Any

(define (stringtree-template string-tree)
  (... (first string-tree) ...
       (list-of-stringtree-template (second string-tree))...))

;; list-of-stringtree-template: (listof StringTree) -> Any

(define (list-of-stringtree-template list-of-stringtree)
  (cond [(empty? list-of-stringtree) ...]
        [else
         (... (stringtree-template (first list-of-stringtree)) ...
              (list-of-stringtree-template (rest list-of-stringtree)) ...)]))

;; example:
(check-expect (stringtree-insert "Daigo" (list 2 1 1 0) celebs)
              (list
               "Punk"
               (list
                (list "Tokido" '())
                (list
                 "Marn"
                 (list
                  (list "JWong" '())
                  (list "Ricki" '())
                  (list "KBrad" '())))
                (list
                 "Smug"
                 (list
                  (list "Fuudo" '())
                  (list
                   "Mena"
                   (list
                    (list "Caba" '())
                    (list "NuckleDu" (list (list "Daigo" '())))))
                  (list "SnakeEyez" '()))))))
                    
(define (stringtree-insert str insertion-path stringtree)
  (cond [(empty? insertion-path)
         (error "invalid path")]
        [(and (empty? stringtree)
              (or (> (length insertion-path) 1)
                  (> (first insertion-path) 0)))
         (error "invalid path")]
        [(empty? stringtree) (list str '())]
        [else (cons (first stringtree)
                    (list (list-of-stringtree-insert
                           str
                           insertion-path
                           (second stringtree))))]))

(define (list-of-stringtree-insert str insertion-path list-of-stringtree)
  (cond [(empty? list-of-stringtree)
         (cons (stringtree-insert str
                                  insertion-path
                                  empty) empty)]
        [(> (first insertion-path) (length list-of-stringtree))
         (error "invalid path")]
        [(= (first insertion-path) 0)
         (cons (stringtree-insert str
                                  (rest insertion-path)
                                  (first list-of-stringtree))
               (rest list-of-stringtree))]
        [else (cons (first list-of-stringtree)
                    (list-of-stringtree-insert
                     str
                     (cons (sub1 (first insertion-path))
                           (rest insertion-path))
                     (rest list-of-stringtree)))]))

;; tests:
(check-expect (stringtree-insert "grains" (list 0 2) big-example-tree)
              '("food" (("plant" (("fruit" (("apple" ())
                                            ("banana" ())))
                                  ("veggie" ())
                                  ("grains" ())))
                        ("animal" (("dairy" (("milk"
                                              (("cheese"
                                                (("pizza" ())))))))
                                   ("meat" (("beef" ())
                                            ("chicken" ())
                                            ("pork" ())
                                            ("lamb" ()))))))))

(check-expect (stringtree-insert "turkey" (list 1 1 4) big-example-tree)
              '("food" (("plant" (("fruit" (("apple" ())
                                            ("banana" ())))
                                  ("veggie" ())))
                        ("animal" (("dairy" (("milk"
                                              (("cheese"
                                                (("pizza" ())))))))
                                   ("meat" (("beef" ())
                                            ("chicken" ())
                                            ("pork" ())
                                            ("lamb" ())
                                            ("turkey" ()))))))))

(check-expect (stringtree-insert "dalmation" (list 0 0 0 0) small-example-tree)
              '("animal"
                (("dog"
                  (("gold"
                    (("retriever"
                      (("dalmation" ()))))))))))

(check-error (stringtree-insert "pineapple" (list 3) big-example-tree)
             "invalid path")

(check-error (stringtree-insert "salmon" (list 1 1 5) big-example-tree)
             "invalid path")

(check-error (stringtree-insert "bagel" empty big-example-tree)
             "invalid path")

(check-error (stringtree-insert "cat" (list 2) small-example-tree)
             "invalid path")

(check-error (stringtree-insert "fish" (list 0 0 0 0 0 0 0) small-example-tree)
             "invalid path")


                                 




