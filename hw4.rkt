
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
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let* ([l (length xs)]
                  [r (remainder n l)])
              (car (list-tail xs r)))]))


;problem 4
(define (stream-for-n-steps s n)
  (if (> n 0)
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
      null))


;problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda()(f (+ 1 x))))
                              (cons x (lambda () (f (+ 1 x))))))])
           (lambda () (f 1))))


;problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cond [x(cons "dan.jpg" (lambda () (f (not x))))]
                                [#t (cons "dog.jpg" (lambda () (f (not x))))]))])
           (lambda() (f #t))))


;problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x)))
                                (lambda () (f (cdr (x))))))])
    (lambda () (f s))))


;problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x)
                                      (list-nth-mod ys x))
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))


;problem 9
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (n v) (cond [(>= n l) #f]
                                  [(not (pair? (vector-ref vec n)))
                                   (f ( + 1 n) v)]
                                  [(equal? v (car (vector-ref vec n)))
                                   (vector-ref vec n)]
                                  [#t (f (+ 1 n) v)]))])
    (f 0 v)))


;problem 10
(define (cached-assoc xs n)
  (let* ([cache (make-vector n null)]
         [i 0])
    (lambda (v) (let ([ans (assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! cache i new-ans)
                          (set! i (remainder (+ i 1) n))
                          new-ans)))))))
         


