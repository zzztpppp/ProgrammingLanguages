;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlst)
  (if (null? rlst)
      (aunit)
      (apair (car rlst) (racketlist->mupllist (cdr rlst)))))

(define (mupllist->racketlist mlst)
  (if (aunit? mlst)
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))
      
  

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        ;; Evaluate values to themselves
        [(or (int? e) (or (closure? e) (or (aunit? e) (pair? e)))) e]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if ( > (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (eval-under-env (ifgreater-e4 e) env)))]
        [(fun? e) (closure env e)]

        [(mlet? e) (let ([v (mlet-var e)]
                         [e1 (mlet-e e)]
                         [b (mlet-body e)])
                         (eval-under-env b (cons (cons v (eval-under-env e1 env))
                                                 env)))]

        [(call? e) (let ([clsr (eval-under-env (call-funexp e) env)]
                         [arg (eval-under-env (call-actual e) env)])
                     (if (closure? clsr)
                         ;Extend the envirement with argument
                         (let ([env-ext (cons (cons (fun-formal (closure-fun clsr)) arg) (closure-env clsr))]
                               [exp (fun-body (closure-fun clsr))])
                           
                           ;Extend the envirement to the function name mapped the closure if
                           ; the function has a name.
                           (eval-under-env exp (if (fun-nameopt (closure-fun clsr))
                                                   (cons (cons (fun-nameopt (closure-fun clsr)) clsr) env-ext)
                                                   env-ext)))
                         (error (format "bad function call: ~v" e))))]
        [(apair? e) (let ([e1 (apair-e1 e)]
                          [e2 (apair-e2 e)])
                      (apair (eval-under-env e1 env) (eval-under-env e2 env)))]

        [(fst? e) (let ([r (eval-under-env (fst-e e) env)])
                    (if (apair? r) (apair-e1 r) (error "can only evaluate fst on a pair")))]
        [(snd? e) (let ([r (eval-under-env (snd-e e) env)])
                    (if (apair? r) (apair-e2 r) (error "can only evaluate snd on a pair")))]

        [(isaunit? e) (let ([r (eval-under-env (isaunit-e e) env)])
                        (if (aunit? r) (int 1) (int 0)))]

                                      
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))


      
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([p (car lstlst)])
        (mlet (car p) (cdr p) (mlet* (cdr lstlst) e2)))))
  
    

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))
         

;; Problem 4

(define mupl-map
  (fun #f "f" (fun "map" "ml" (ifeq (int 1) (isaunit (var "ml"))
                                    (aunit)
                                    (apair (call (var "f") (fst (var "ml")))
                                           (call (var "map") (snd (var "ml")))))))) 
                    
                             

(define mupl-mapAddN 
  (mlet "map" mupl-map (fun #f "i" (call mupl-map (fun #f "x" (add (var "i") (var "x"))))))) 
        

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct rest (e v))
  (define (helper-funcs e vars)
    (cond ([(var? e) (rest (e 
                                                                 
                                                                 
                         
                                                             

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c  e env) "cHANGE" )

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
