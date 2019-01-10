;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname immigration) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 02, Problem 2
;; ************************************************************************
;;

;; breakpoints and maximum point constants for age function
(define agebp1 18)
(define agebp2 20)
(define agebp3 29)
(define agebp4 49)
(define max-p 100)

;; point values for education function
(define grad-p 126)
(define undergrad-p 112)
(define high-school-p 28)

;; point values for language function
(define high-lang-p 116)
(define mid-lang-p 88)
(define low-lang-p 64)

;; point values for work function
(define beginner 35)
(define intermediate 56)
(define veteran 70)

;; point value for offer function
(define offer-points 200)

;; required points for CEC eligibility
(define required-points 350)

;; (age n) produces the number of points given for an age n
;; age: Num -> Num
;; requires: n >= 0
;; example:
(check-expect (age 27) 100)

(define (age n) (cond [(and (>= n agebp1) (< n agebp2))
                       (- max-p (* (- agebp2 n) 5))]
                      [(and (>= n agebp2) (<= n agebp3)) max-p]
                      [(and (> n agebp3) (<= n agebp4))
                       (- max-p (* 5 (- n agebp3)))]
                      [else 0]))

;;tests:
(check-expect (age 16) 0)
(check-expect (age 18) 90)
(check-expect (age 19) 95)
(check-expect (age 20) 100)
(check-expect (age 25) 100)
(check-expect (age 29) 100)
(check-expect (age 35) 70)
(check-expect (age 49) 0)
(check-expect (age 70) 0)

;; (education degree) produces the points
;; given for an education level degree
;; education: Sym -> Num
;; requires: degree must be 'graduate, 'undergraduate, or 'highschool
;; example:
(check-expect (education 'graduate) 126)

(define (education degree) (cond [(symbol=? degree 'graduate) grad-p]
                            [(symbol=? degree 'undergraduate) undergrad-p]
                            [(symbol=? degree 'highschool) high-school-p]))

;; tests:
(check-expect (education 'undergraduate) 112)
(check-expect (education 'highschool) 28)

;; (language lang) produces the points given the language proficiency
;; represented with an integer lang from 1 to 10
;; language: Num -> Num
;; requires: lang <= 10
;;           lang > 0
;; example:
(check-expect (language 4) 0)

(define (language lang) (cond [(>= lang 9) high-lang-p]
                           [(= lang 8) mid-lang-p]
                           [(= lang 7) low-lang-p]
                           [else 0]))

;; tests:
(check-expect (language 10) 116)
(check-expect (language 9) 116)
(check-expect (language 8) 88)
(check-expect (language 7) 64)
(check-expect (language 1) 0)

;; (work y) prduces the points given the amount of work
;; experience in years
;; work: Num -> Num
;; requires: years >= 0
;; example:
(check-expect (work 3) 56)

(define (work years) (cond [(= years 1) beginner]
                       [(or (= years 2) (= years 3)) intermediate]
                       [(>= years 4) veteran]
                       [else 0]))

;; tests:
(check-expect (work 1) 35)
(check-expect (work 2) 56)
(check-expect (work 4) 70)
(check-expect (work 8) 70)
(check-expect (work 0) 0)

;; (offer offer-exists?) produces the points given if the applicant
;; has received a job offer, denoted by offer-exists?
;; offer: Bool -> Num
;; requires: offer-exists? must be true or false
;; example:
(check-expect (offer true) 200)

(define (offer offer-exists?)
  (cond [offer-exists? offer-points] [else 0]))

;; tests:
(check-expect (offer false) 0)

;; (pr-cec-score n degree lang years offer-exists?) determines an immigrants
;; status given the age n, education level degree, language proficiency
;; lang, years spent working, and job offer offer-exists?
;; pr-cec-score: Num Sym Num Num Bool -> Num
;; requires: n >= 0
;;           degree must be 'graduate, 'undergraduate, or 'highschool
;;           lang <= 10
;;           lang > 0
;;           years > 0
;;           offer-exists? must be true or false
;; example:
(check-expect (pr-cec-score 27 'undergraduate 3 1 false) 247)

(define (pr-cec-score n degree lang years offer-exists?)
  (+ (age n) (education degree) (language lang)
     (work years) (offer offer-exists?)))

;; tests:
(check-expect (pr-cec-score 70 'undergraduate 1 0 false) 112)
(check-expect (pr-cec-score 49 'undergraduate 1 0 false) 112)
(check-expect (pr-cec-score 35 'undergraduate 1 0 false) 182)
(check-expect (pr-cec-score 29 'undergraduate 1 0 false) 212)
(check-expect (pr-cec-score 25 'undergraduate 1 0 false) 212)
(check-expect (pr-cec-score 20 'undergraduate 1 0 false) 212)
(check-expect (pr-cec-score 19 'undergraduate 1 0 false) 207)
(check-expect (pr-cec-score 18 'undergraduate 1 0 false) 202)
(check-expect (pr-cec-score 16 'undergraduate 1 0 false) 112)           
(check-expect (pr-cec-score 70 'graduate 1 0 false) 126) 
(check-expect (pr-cec-score 70 'highschool 1 0 false) 28)
(check-expect (pr-cec-score 70 'undergraduate 7 0 false) 176)
(check-expect (pr-cec-score 70 'undergraduate 8 0 false) 200)
(check-expect (pr-cec-score 70 'undergraduate 9 0 false) 228)
(check-expect (pr-cec-score 70 'undergraduate 10 0 false) 228)
(check-expect (pr-cec-score 70 'undergraduate 1 1 false) 147)
(check-expect (pr-cec-score 70 'undergraduate 1 2 false) 168)
(check-expect (pr-cec-score 70 'undergraduate 1 4 false) 182)
(check-expect (pr-cec-score 70 'undergraduate 1 8 false) 182)
(check-expect (pr-cec-score 70 'undergraduate 1 0 true) 312)

;; (pr-cec-eligible? n degree lang years offer-exists?) determines an immigrants
;; status given the age n, education level degree, language proficiency
;; lang, years spent working, and job offer offer-exists?
;; pr-cec-eligible?: Num Sym Num Num Bool -> Num
;; requires: n >= 0
;;           degree must be 'graduate, 'undergraduate, or 'highschool
;;           lang <= 10
;;           lang > 0
;;           years > 0
;;           offer-exists? must be true or false
;; example:
(check-expect (pr-cec-eligible? 27 'undergraduate 3 1 false) false)

(define (pr-cec-eligible? n degree lang years offer-exists?)
  (cond [(and (cond [(>= years 1) true]
                     [else false])
               (>= (+ (age n) (education degree)
                      (language lang) (work years)
                      (offer offer-exists?)) required-points))
         true]
        [else false]))

;; tests:
(check-expect (pr-cec-eligible? 70 'undergraduate 1 0 false) false)
(check-expect (pr-cec-eligible? 29 'graduate 7 3 true) true)
(check-expect (pr-cec-eligible? 24 'graduate 10 4 true) true)