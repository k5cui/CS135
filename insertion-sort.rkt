;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname insertion-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (insert element list)
  (cond [(empty? list) (cons element empty)]
        [(<= element (first list))
         (cons element list)]
        [else (cons (first list) (insert element (rest list)))]))

(define (sort list)
  (cond [(empty? list) empty]
        [else (insert (first list) (sort (rest list)))]))


(check-expect (sort (cons 2 (cons 4 (cons 3 empty))))
              (cons 2 (cons 3 (cons 4 empty))))