
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


;problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;problem 3
(define (list-nth-mod xs n)
  (cond [(<0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let l 
  


