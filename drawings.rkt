;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname drawings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a07drawinglib.rkt")

;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 07, Problem 2
;; ************************************************************************
;;

;; A Coordinate is a (make-posn Int Int)
;; An ImageColor is a Str
;; requires: the Str is from the racket/draw colour database:
;; https://docs.racket-lang.org/draw/color-database___.html

;; A PrimCircle is a (make-prim-circle Coordinate Nat ImageColor)

;; A PrimTriangle is a (make-prim-triangle
;; Coordinate Coordinate Coordinate ImageColor)

;; A PrimElement is (anyof PrimTriangle PrimCircle)

;; (render-image image-size prim-shapes) produces an
;; Image on an image of size image-size containing the list of
;; shapes listed by prim-shapes.
;; render-image: Posn (listof PrimElements) -> Image
;; requires: canvas-size is a (make-posn Nat Nat)

(define prim-picture
  (list
  (make-prim-circle (make-posn 178 157) 8 "Black")
  (make-prim-circle (make-posn 122 157) 8 "Black")
  (make-prim-triangle (make-posn 147 177) (make-posn 150 182)
                      (make-posn 153 177) "Black")
  (make-prim-triangle (make-posn 100 70) (make-posn 100 40)
                      (make-posn 109 65) "Black")
  (make-prim-triangle (make-posn 200 70) (make-posn 200 40)
                      (make-posn 191 65) "Black")
  (make-prim-triangle (make-posn 100 130)(make-posn 100 40)
                      (make-posn 130 120) "Gold")
  (make-prim-triangle (make-posn 200 130) (make-posn 200 40)
                      (make-posn 170 120) "Gold")
  (make-prim-circle (make-posn 195 190) 13 "Tomato")
  (make-prim-circle (make-posn 105 190) 13 "Tomato")
  (make-prim-circle (make-posn 150 175) 70 "Gold")))

;; (rect a b colour) creates a pair of triangles that form a coloured
;; rectangle with width posn-x b and height posn-y b from a
;; rect: Posn Posn Str -> (listof PrimTriangle)
;; example:
(check-expect (rect (make-posn 10 10) (make-posn 10 10) "Yellow")
              (list
               (make-prim-triangle (make-posn 10 10)
                                   (make-posn 20 10)
                                   (make-posn 20 20) "Yellow")
               (make-prim-triangle (make-posn 10 10)
                                   (make-posn 20 20)
                                   (make-posn 10 20) "Yellow")))

(define (rect a b colour)
  (list
   (make-prim-triangle a
                       (make-posn (+ (posn-x a) (posn-x b))
                                  (posn-y a))
                       (make-posn (+ (posn-x a) (posn-x b))
                                  (+ (posn-y a) (posn-y b))) colour)
   (make-prim-triangle a
                       (make-posn (+ (posn-x a) (posn-x b))
                                  (+ (posn-y a) (posn-y b)))
                       (make-posn (posn-x a)
                                  (+ (posn-y a) (posn-y b))) colour)))

;; tests:
(check-expect (rect (make-posn 0 0) (make-posn 10 10) "Blue")
              (list
               (make-prim-triangle (make-posn 0 0)
                                   (make-posn 10 0)
                                   (make-posn 10 10) "Blue")
               (make-prim-triangle (make-posn 0 0)
                                   (make-posn 10 10)
                                   (make-posn 0 10) "Blue")))
(check-expect (rect (make-posn 0 0) (make-posn 0 10) "Blue")
              (list
               (make-prim-triangle (make-posn 0 0)
                                   (make-posn 0 0)
                                   (make-posn 0 10) "Blue")
               (make-prim-triangle (make-posn 0 0)
                                   (make-posn 0 10)
                                   (make-posn 0 10) "Blue")))

;; A Point is a (list Int Int)
;; An Offset is a Point

;; A ShapeID is a Sym
;; requires: ShapeID is not 'circle, 'triangle, 'rectangle, 'component

;; A Shape is one of:
;; - (list 'circle ShapeID radius ImageColor)
;; - (list 'triangle ShapeID Point Point Point ImageColor)
;; - (list 'rectangle ShapeID width height ImageColor)
;; - (list 'component ShapeID Picture)
;; requires: radius,width,height are Nat
;;           The ShapeID of a component does not appear in its Picture
;;           when recursively expanded.
;;           (i.e. there are no circular definitions)

;; A Picture is a (listof (list Offset ShapeID))
;; A ShapeList is a (listof Shape)
;; requires: every ID in the ShapeList is unique

;; A BundledDrawing is a (list width height Picture ShapeList)
;; requires: width, height are Nat
;;           Every ShapeID in the Picture occurs in ShapeList.

(define summer-shapes '((circle top-scoop 10 "Pink")
                        (circle bottom-scoop 10 "LightBlue")
                        (component ice-cream
                                   (((0 40) cone)
                                    ((10 35) bottom-scoop)
                                    ((10 25) top-scoop)))
                        (triangle cone (0 0) (20 0) (10 50) "Burlywood")
                        (circle moon 35 "Light Gray")
                        (circle sun 40 "Yellow")))

(define ice-cream-pic '(((10 50) ice-cream)
                        ((70 20) sun)
                        ((130 30) ice-cream)))

(define fun-shapes '((circle leaves 10 "Green")
                     (rectangle trunk 5 20 "Brown")
                     (triangle roof (20 0) (0 10) (40 10) "Khaki")
                     (rectangle wall 40 35 "DarkKhaki")
                     (circle glass 5 "White")
                     (rectangle horizontal-bar 8 2 "Black")
                     (rectangle vertical-bar 2 8 "Black")
                     (rectangle door 8 10 "RosyBrown")
                     (component tree-pic (((35 10) trunk)
                                          ((37 0) leaves)))
                     (component window-pic (((3 0) vertical-bar)
                                            ((0 3) horizontal-bar)
                                            ((4 4) glass)))
                     (component house-pic (((5 35) door)
                                           ((15 25) window-pic)
                                           ((0 0) roof)
                                           ((0 10) wall)))))
(define fun-pic '(((53 61) window-pic)
                  ((20 65) tree-pic)
                  ((75 50) house-pic)))

(define fun-drawing (list 200 200 fun-pic fun-shapes))

(define ice-cream-drawing (list 200 150 ice-cream-pic summer-shapes))

;; (find-shape id shape-list) produces the shape of a shape-id given a
;; shape-list
;; find-shape: ShapeID (listof Shape) -> Shape
;; requires: id must be exist in shape-list

(define (find-shape id shape-list)
  (cond [(symbol=? id (shape-id (first shape-list)))
         (first shape-list)]
        [else (find-shape id (rest shape-list))]))

;; (get-all-ids pic shape-list) produces a list of every shape-id that occurs in
;; a pic with repeats
;; get-all-ids: Picture (listof Shape) -> (listof ShapeID)
;; requires: all ShapeIDs in the pic must be found in the shape-list

(define (get-all-ids pic shape-list)
  (cond [(empty? pic) empty]
        [else (local
                [(define current-shape (find-shape (second (first pic))
                                                   shape-list))]
                (cond
                  [(symbol=? (shape-type current-shape) 'component)
                   (append (cons (shape-id current-shape)
                                 (get-all-ids (component-picture current-shape)
                                              shape-list))
                           (get-all-ids (rest pic) shape-list))]
                  [else (cons (second (first pic))
                              (get-all-ids (rest pic)
                                           shape-list))]))]))

;; (insert-id id id-list) adds an element to a list if it does not already exist
;; in it
;; insert-id: ShapeID (listof ShapeID) -> (listof ShapeID)

(define (insert-id id id-list)
  (cond [(empty? id-list) (cons id empty)]
        [(not (member? id id-list)) (cons id id-list)]
        [else id-list]))

;; (new-list id-list) takes an id-list and removes all the duplicate IDs
;; new-list: (listof ShapeID) -> (listof ShapeID)

(define (new-list id-list)
  (cond [(empty? id-list) empty]
        [else (insert-id (first id-list) (new-list (rest id-list)))]))

;; (get-picture-ids pic shape-list) produces a list of every shape-id that
;; occurs in a pic without repeats
;; get-all-ids: Picture (listof Shape) -> (listof ShapeID)
;; requires: all ShapeIDs in the pic must be found in the shape-list
;; example:
(check-expect (get-picture-ids ice-cream-pic summer-shapes)
              (list 'sun 'ice-cream 'cone 'bottom-scoop 'top-scoop))

(define (get-picture-ids pic shape-list)
  (new-list (get-all-ids pic shape-list)))

;; tests:
(check-expect (get-picture-ids fun-pic fun-shapes)
              (list 'tree-pic 'trunk 'leaves 'house-pic 'door 'window-pic
                    'vertical-bar 'horizontal-bar 'glass 'roof 'wall))

(check-expect (get-picture-ids '(((0 0) tree-pic)
                                 ((10 10) tree-pic))
                               fun-shapes)
              (list 'tree-pic 'trunk 'leaves))

;; (add-offset off-set pic) produces a picture with a offset added onto
;; each of the existing offsets
;; add-offset: Picture -> Picture

(define (add-offset off-set pic)
  (cond [(empty? pic) empty]
        [else (cons (list (list (+ (first (first (first pic)))
                         (first off-set))
                      (+ (second (first (first pic)))
                         (second off-set)))
                     (second (first pic)))
                    (add-offset off-set (rest pic)))]))

;; (picture->primitives pic shape-list) produces a list of PrimElements
;; that can be rendered in an image from a Picture
;; picture->primitives: Picture (listof Shape) -> (listof PrimElement)
;; requires: all ShapeIDs in the pic can be found in the shape-list
;; example:
(check-expect (picture->primitives ice-cream-pic summer-shapes)
(list
 (make-prim-triangle (make-posn 10 90) (make-posn 30 90)
                     (make-posn 20 140) "Burlywood")
 (make-prim-circle (make-posn 20 85) 10 "LightBlue")
 (make-prim-circle (make-posn 20 75) 10 "Pink")
 (make-prim-circle (make-posn 70 20) 40 "Yellow")
 (make-prim-triangle (make-posn 130 70) (make-posn 150 70)
                     (make-posn 140 120) "Burlywood")
 (make-prim-circle (make-posn 140 65) 10 "LightBlue")
 (make-prim-circle (make-posn 140 55) 10 "Pink")))

(define (picture->primitives pic shape-list)
  (cond [(empty? pic) empty]
        [else
         (local
           [(define current-shape (find-shape (second (first pic))
                                              shape-list))
            (define current-type (shape-type current-shape))]
           (cond [(symbol=? current-type 'triangle)
                  (cons (make-prim-triangle
                         (make-posn
                          (+ (first (first (first pic)))
                             (first (triangle-p1 current-shape)))
                          (+ (second (first (first pic)))
                             (second (triangle-p1 current-shape))))
                         (make-posn
                          (+ (first (first (first pic)))
                             (first (triangle-p2 current-shape)))
                          (+ (second (first (first pic)))
                             (second (triangle-p2 current-shape))))
                         (make-posn
                          (+ (first (first (first pic)))
                             (first (triangle-p3 current-shape)))
                          (+ (second (first (first pic)))
                             (second (triangle-p3 current-shape))))
                         (triangle-color current-shape))
                        (picture->primitives (rest pic) shape-list))]
                 
                 [(symbol=? current-type 'circle)
                  (cons (make-prim-circle
                         (make-posn
                          (first (first (first pic)))
                          (second (first (first pic))))
                         (circle-radius current-shape)
                         (circle-color current-shape))
                        (picture->primitives (rest pic) shape-list))]
                 
                 [(symbol=? current-type 'rectangle)
                  (append (rect (make-posn (first (first (first pic)))
                                           (second (first (first pic))))
                                (make-posn (rect-width current-shape)
                                           (rect-height current-shape))
                                (rect-color current-shape))
                          (picture->primitives (rest pic) shape-list))]
                 
                 [(symbol=? current-type 'component)
                  (append (picture->primitives
                           (add-offset (first (first pic))
                                       (component-picture current-shape))
                                               shape-list)
                          (picture->primitives (rest pic) shape-list))]))]))

;; tests:
(check-expect (picture->primitives fun-pic fun-shapes)
(list
 (make-prim-triangle (make-posn 56 61) (make-posn 58 61)
                     (make-posn 58 69) "Black")
 (make-prim-triangle (make-posn 56 61) (make-posn 58 69)
                     (make-posn 56 69) "Black")
 (make-prim-triangle (make-posn 53 64) (make-posn 61 64)
                     (make-posn 61 66) "Black")
 (make-prim-triangle (make-posn 53 64) (make-posn 61 66)
                     (make-posn 53 66) "Black")
 (make-prim-circle (make-posn 57 65) 5 "White")
 (make-prim-triangle (make-posn 55 75) (make-posn 60 75)
                     (make-posn 60 95) "Brown")
 (make-prim-triangle (make-posn 55 75) (make-posn 60 95)
                     (make-posn 55 95) "Brown")
 (make-prim-circle (make-posn 57 65) 10 "Green")
 (make-prim-triangle (make-posn 80 85) (make-posn 88 85)
                     (make-posn 88 95) "RosyBrown")
 (make-prim-triangle (make-posn 80 85) (make-posn 88 95)
                     (make-posn 80 95) "RosyBrown")
 (make-prim-triangle (make-posn 93 75) (make-posn 95 75)
                     (make-posn 95 83) "Black")
 (make-prim-triangle (make-posn 93 75) (make-posn 95 83)
                     (make-posn 93 83) "Black")
 (make-prim-triangle (make-posn 90 78) (make-posn 98 78)
                     (make-posn 98 80) "Black")
 (make-prim-triangle (make-posn 90 78) (make-posn 98 80)
                     (make-posn 90 80) "Black")
 (make-prim-circle (make-posn 94 79) 5 "White")
 (make-prim-triangle (make-posn 95 50) (make-posn 75 60)
                     (make-posn 115 60) "Khaki")
 (make-prim-triangle (make-posn 75 60) (make-posn 115 60)
                     (make-posn 115 95) "DarkKhaki")
 (make-prim-triangle (make-posn 75 60) (make-posn 115 95)
                     (make-posn 75 95) "DarkKhaki")))

;; (drawing->image bundled-draw) renders an image given a bundled-draw
;; drawing->image: BundledDrawing -> Image

(define (drawing->image bundled-draw)
  (render-image (make-posn
                 (first bundled-draw)
                 (second bundled-draw))
                (picture->primitives (third bundled-draw)
                                     (fourth bundled-draw))))

                               