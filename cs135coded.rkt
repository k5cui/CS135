;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135coded) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ************************************************************************
;;   Kenneth Cui (20762994)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 2
;; ************************************************************************
;;

;; (get-char string position) produces the char value located at a certain
;; position of a string
;; get-char: Str Int -> Char
;; requires: position >= 0
;; example:
(check-expect (get-char "abcdefg" 0) #\a)

(define (get-char string position)
  (cond [(>= position (length (string->list string))) #\*]
        [(= position 0)
         (first (string->list string))]
        [else (get-char (list->string (rest (string->list string)))
                        (sub1 position))]))
;; tests:
(check-expect (get-char "ihave3apples" 8) #\p)
(check-expect (get-char "compsci" 2) #\m)
(check-expect (get-char "banana" 5) #\a)
(check-expect (get-char "4" 6) #\*)

;; A Decryptor is a (list Nat Nat Nat)

;; (coded-3-char) produces a string containing the 3-character “secret message”
;; hidden in the consumed string.
;; coded-3-char: Str Decryptor -> Str
;; example:
(check-expect (coded-3-char "abcdefg" (list 0 0 0)) "abc")

(define (coded-3-char string decryptor)
  (cond [(empty? (rest decryptor))
                 (list->string (cons (get-char string (first decryptor))
                                     empty))]
        [else (list->string
               (cons (get-char string (first decryptor))
                     (string->list (coded-3-char
                     string
                     (cons (+ 1 (first decryptor) (second decryptor))
                           (rest (rest decryptor)))))))])) 

;; tests:
(check-expect (coded-3-char "abcdefg" (list 0 2 1)) "adf")
(check-expect (coded-3-char "cs135" (list 0 5 10)) "c**")
(check-expect (coded-3-char "dududududu" (list 2 1 3)) "ddd")
(check-expect (coded-3-char "abcdefg" (list 10 2 1)) "***")

;; (enc-possible? string decrypted) determines if it is possible to hide a
;; decrypted code in a string
;; enc-possible?: Str Str -> Bool
;; requires: decrypted must be 3 letters long and not contain *
;; example:
(check-expect (enc-possible? "abcdefg" "adf") true)

(define (enc-possible? string decrypted)
  (cond [(empty? (string->list decrypted)) true]
        [(empty? (string->list string)) false]
        [(char=? (first (string->list string))
                 (first (string->list decrypted)))
         (enc-possible? (list->string (rest (string->list string)))
                        (list->string (rest (string->list decrypted))))]
        [else (enc-possible? (list->string (rest (string->list string)))
                             decrypted)]))

;;tests
(check-expect (enc-possible? "abcdefg" "efg") true)
(check-expect (enc-possible? "abcdefg" "adj") false)
(check-expect (enc-possible? "abcdefg" "daf") false)
(check-expect (enc-possible? "abcdefg" "abcdefgh") false)
(check-expect (enc-possible? "abcdefg" "") true)


                                       
