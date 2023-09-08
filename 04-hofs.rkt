#lang racket

(require racket/trace)


#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:

- `apply`: apply a function to a list of arguments

- `curry`: returns a version of a function that can be partially applied

- `compose`: returns a function that is the composition of two other functions

- `eval`: evaluates a sexp
-----------------------------------------------------------------------------|#

;; `apply` applies a function to lists

 (+ 1 2 3) ; -> 6
 (define args '(1 2 3))
; (+ args) ; -> fails, need numbers, not lists
 (apply + args) ; -> 6
 (apply + '(1 2 3)) ; -> 6
 (apply + 1 2 '(3 4)) ; -> 10
 (apply cons '(1 2)) ; -> '(1 . 2)
 (define (sum . xs)
   (apply + xs))

;; `curry` gives us partial application

; (cons 1) -> fails
; (cons) -> fails
(curry cons)
(define ccons (curry cons))
cons
ccons
(ccons 1)
((ccons 1) 2)

(define conswith3 (curry cons 3))

(define (arity-of-3 x y z) ; 3 args
  (* x (+ y z)))

(define foo (curry arity-of-3 5)) ; (foo 10 20) -> 150 ~> 5*(10+20)

(define (flip f)
  (lambda (x y) (f y x)))
; (cons 1 2) -> '(1 . 2)
; ((flip cons) 1 2) -> '(2 . 1)

;; compose is a simple but powerful form of "functional "glue"

; basically the F(G(x)) : F O G
((compose sqrt abs) -4) ; == (sqrt(abs -1)) == 2

(define (my-compose f g) ; make a function with args f and g
  (lambda (x) ; return a function with 1 args
    (f (g x)))) ; lambda returns this <-

(define even?
  (my-compose (curry = 0)
              (curry (flip remainder) 2)))
(even? 5)
(even? 8)

; do (define function ... if no args
; do (define (function args) ... if args
  

;; eval is like having access to the Racket compiler in Racket!
; (eval `(cons '+ (cons 1 (cons 2 '()))))

(define (my-if test e1 e2)
  (eval `(cond (,test , e1)
               (else , e2))))

(my-if '(< 1 2)
       '(println "true")
       '(println "false"))

(define (three-times e)
  (eval `(begin, e, e, e)))



#|-----------------------------------------------------------------------------
;; Some list-processing HOFs

- `map`: applies a function to every element of a list, returning the results

- `filter`: collects the values of a list for which a predicate tests true

- `foldr`: implements *primitive recursion*

- `foldl`: like `foldr`, but folds from the left; tail-recursive
-----------------------------------------------------------------------------|#

; NOTES:
; functions only run if at first position of a list
; ((lambda () 1)) will print 1
; (lambda () 1) wont actually run the lambda function

;; `map` examples
#; (values
   (map add1 (range 10))

   (map (curry * 2) (range 10))
 
   (map string-length '("hello" "how" "is" "the" "weather?")))


;; `filter` examples
#; (values 
   (filter even? (range 10))
   
   (filter (curry < 5) (range 10))

   (filter (compose (curry equal? "hi")
                    car)
           '(("hi" "how" "are" "you")
             ("see" "you" "later")
             ("hi" "med" "low")
             ("hello" "there"))))


;; `foldr` examples
#; (values
    (foldr + 0 (range 10))

    (foldr cons '() (range 10))

    (foldr cons '(a b c d e) (range 5))

    (foldr (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))


;; `foldl` examples
#; (values
    (foldl + 0 (range 10))
    
    (foldl cons '() (range 10))
    
    (foldl cons '(a b c d e) (range 5))
    
    (foldl (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))



#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#