;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; (occurences lon n) produces the number of times a number appears in a list
;; occurences: (listof Num) Num -> Nat
;; example:
(check-expect (occurrences '(1 3 4 5 5 2 5 5) 5) 4)

(define (occurrences lon n)
  (length (filter (lambda (a) (= a n)) lon)))

;; tests:
(check-expect (occurrences '(1 1 1 1 1) 1) 5)
(check-expect (occurrences '(1 1 1 1 1) 2) 0)
(check-expect (occurrences '(1 2 99 5 0) 1) 1)
(check-expect (occurrences '(1 2 99 5 0) 99) 1)
(check-expect (occurrences '(1 2 99 5 0) 0) 1)

;; (absolutely-dd lon) takes the sum of the absolute value of all the odd
;; values in a list of integers
;; absolutely-odd: (listof Int) -> Nat
;; example:
(check-expect (absolutely-odd '(1 3 5 7)) 16)

(define (absolutely-odd lon)
  (foldr (lambda (a b) (+ (abs a) b)) 0 (filter odd? lon)))

;; tests:
(check-expect (absolutely-odd '(-1 -3 -5 -7)) 16)
(check-expect (absolutely-odd '(2)) 0)
(check-expect (absolutely-odd '(1 2 3 4)) 4)
(check-expect (absolutely-odd '(-1 2 3 4)) 4)
(check-expect (absolutely-odd '(-2 -4 -5)) 5)

;; (zip list-one list-two) combines the elements of 2 lists so that each
;; element with the same index is stored in one list
;; zip: (listof Any) (listof Any) -> (listof (listof Any))
;; requires: both the lists must be the same length
;; example: 
(check-expect (zip '(a b c) '(d e f)) '((a d) (b e) (c f)))

(define (zip list-one list-two)
  (map (lambda (x y) (list x y)) list-one list-two))

;; tests:
(check-expect (zip '() '()) '())
(check-expect (zip '(1 a b "3") '(c d "w" 5))
              '((1 c) (a d) (b "w") ("3" 5)))
(check-expect (zip '((1 2) 3 4) '(a b c))
              '(((1 2) a) (3 b) (4 c)))

;; (unzip lst) takes apart a list of lists to form 2 lists
;; unzip: (listof (listof Any)) -> (list (listof Any) (listof Any))
;; requires: each list in lst may only contain 2 elements
;; example:
(check-expect (unzip '((a d) (b e) (c f))) '((a b c) (d e f)))

(define (unzip lst)
  (list (map first lst) (map second lst)))

;; tests:
(check-expect (unzip '()) '(() ()))
(check-expect (unzip '((1 c) (a d) (b "w") ("3" 5)))
              '((1 a b "3") (c d "w" 5)))
(check-expect (unzip '(((1 2) a) (3 b) (4 c))) '(((1 2) 3 4) (a b c)))

;; (dedup lst) takes a list of numbers and only keeps the first time a
;; number is shown
;; dedup: (listof Num) -> (listof Num)
;; example:
(check-expect (dedup '(1 1 2 1 2)) '(1 2))

(define (dedup lst)
  (foldr (lambda (a b)
           (cons a (filter (lambda (c) (not (= a c))) b)))
         empty lst))

;; tests:
(check-expect (dedup '(1 1 1 1 1)) '(1))
(check-expect (dedup '(1 2 3)) '(1 2 3))
(check-expect (dedup '()) '())

;; (subsequence lst start end) produces a subsequence of a list starting from
;; start and ending right before index end
;; subsequence: (listof Any) Nat Nat -> (listof Any)
;; example:
(check-expect (subsequence '(a b c d e f g) 1 4)
              '(b c d))

(define (subsequence lst start end)
 (foldr (lambda (a b c) (cond [(and (>= b start) (< b end)) (cons a c)]
                              [else c]))
        empty lst (build-list (length lst) (lambda (a) a))))

;; tests:
(check-expect (subsequence '(a b c) 1 5)
              '(b c))
(check-expect (subsequence '(1 2 3) 0 1)
              '(1))
(check-expect (subsequence '(a b c) 0 0)
              '())
(check-expect (subsequence '(a b c) 1 1)
              '())
(check-expect (subsequence '(a b c d e f g) 3 99)
              '(d e f g))
(check-expect (subsequence '() 4 5)
              '())

