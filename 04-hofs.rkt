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

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (my-if test e1 e2)
  (eval `(cond (,test ,e1)
               (else ,e2)) ns))

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

(trace-define (map f l)
  (if (empty? l)
      '()
      (cons (f (first l)) ; construct a new list, where each element is the
            (map f (rest l))))) ; result of applying l to an element in l
;; `map` examples
 (values
   (map add1 (range 10))

   (map (curry * 2) (range 10))
 
   (map string-length '("hello" "how" "is" "the" "weather?")))

(trace-define (filter p l)
  (cond [(empty? l) '()]
        [(p (first l)) (cons (first l)
                             (filter p (rest l)))] ; ^ this is plain rec
        [else (filter p (rest l))])) ; <- this is tail end rec
;; `filter` examples
 (values 
   (filter even? (range 10))
   
   (filter (curry < 5) (range 10))

   (filter (compose (curry equal? "hi")
                    car)
           '(("hi" "how" "are" "you")
             ("see" "you" "later")
             ("hi" "med" "low")
             ("hello" "there"))))


;; `foldr` examples ; fold right 'prim rec, right associative, run right first
(define (foldr f val lst) ; our version so we can trace
  (if (empty? lst)
      val
      (f (first lst) (foldr f val (rest lst)))))

(trace foldr)

(values
    (foldr + 0 (range 10))

    (foldr cons '() (range 10))

    (foldr cons '(a b c d e) (range 5))

    (foldr (trace-lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5))) ; change '() to 0, cons to + => sum

#; (define (sum2 lst)
  (if (empty? lst)
      0
      (+ (first lst) (sum2 (rest lst)))))

(define sum2 (curry foldr + 0))

(define (copy-list lst)
  (if (empty? lst)
      '()
      (cons (first lst) (copy-list (rest lst)))))

(foldr + 100 '(8 5))
; (+ 8 (foldr + 100 '(5)))
; (+ 8 (+ 5 (foldr + 100 '())))
; (+ 8 (+ 5 100))
; (+ 8 105)
; 113

;; `foldl` examples ; fold left 'tail rec
(define (foldl f acc lst)
  (if (empty? lst)
      acc
      (foldl f (f (first lst) acc) (rest lst))))
(trace foldl)

(values
    (foldl + 0 (range 10))
    
    (foldl cons '() (range 10))
    
    (foldl cons '(a b c d e) (range 5))
    
    (foldl (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

(define reverse (curry foldl cons '()))
(define sum3(curry foldl + 0))
(sum3 (range 10))

(define (partition x lst) ; split lst into 2 lst, one with values less then x and more with more
  (foldl (lambda (y acc)
           (if (< y x)
               (list (cons y (first acc))
                     (second acc))
               (list (first acc)
                     (cons y (second acc)))))
         '(() ())
         lst))

(partition 5 '(1 8 9 2 3 10 7))
  

#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#

(define (simple n)
  (let ([loc 10])
    (+ n loc)))

(define (make-obj)
  (let ([attr 0])
    (lambda (cmd)
      (case cmd
        ['inc (set! attr (add1 attr))]
        ['dec (set! attr (sub1 attr))]
        ['show (println attr)]))))

(define o1 (make-obj))
(o1 'show)
(o1 'inc)
(o1 'inc)
(o1 'show)
(o1 'dec)
(o1 'show)

; loc var can be saved by functions that req them. lambda will save it sort off