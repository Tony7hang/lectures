#lang racket ; <- determines the language (and syntax) used in this file


#|-----------------------------------------------------------------------------
;; Racket at a glance

- LISP/Scheme dialect

  - LISP = "LISt Processing", 
           "Lots of Insidious, Silly Parentheses", 
           "Lost In a Sea of Parentheses"

- Homoiconic; i.e., shared representation for code & data

- Supports (but does not enforce) functional style

  - First class functions
  - Higher-order functions
  - Anonymous functions

- Dynamically and Strongly typed
  dynamic     vs   static
  int i = 1   vs   i = 1

  - Sister language "Typed Racket" is statically typed

- Pass by value with pointer semantics

- Lexically scoped (vs dynamically scoped)

- Heap-based storage with garbage collection
-----------------------------------------------------------------------------|#

;; a bit of Racket to whet your appetite
(define (quicksort < l)
  (match l
    ['() '()]
    [(cons x xs) 
     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
       (append (quicksort < xs-lt) 
               (list x) 
               (quicksort < xs-gte)))]))

(quicksort < '(5 3 1 4 2))
(quicksort > '(5 3 1 4 2))
(quicksort string<? '("hello" "how" "are" "you?"))

#|-----------------------------------------------------------------------------
;; Basic syntax

The essential syntactic unit of Racket is the *S-expression*, or sexp.

A sexp is either an *atom* or a *list*.

- An atom is one of:

  - a Number 
    - e.g., 42, -23, 3.14, 5/8, 2+3i, 6.02e+23

  - a '#' prefixed value 
    - e.g., booleans #t and #f, chars #\a, #\newline)

  - a String
    - e.g., "hello", "foo\nbar"

  - a Symbol, written as an identifier made of any chars except #()[]{}|\'"`,;
    - e.g., foo, hot-enough?, <=, list->set, call/cc, bang!!!

- A *list* is a sequence of sexps (recursion!) separated by spaces, 
  enclosed by parentheses

  - [] and {} can also be used as delimiters
-----------------------------------------------------------------------------|#

;; Let's write some (syntactically) valid sexps!
;; Note: `#;` is a convenient, special "sexp comment"

42

#; a 45 ; only a is commented out, sexp comment

; a ; syntatically correct but will give error when ran cuz cant be evaled

#; "hello"
#; #f
#; (hi)


#|-----------------------------------------------------------------------------
;; Evaluating sexps

- Numbers, strings, and `#` prefixed values evaluate to themselves

- Symbols are identifiers whose *bindings* are looked up in the current scope

  - If an identifier corresponds to a *variable*, its value is returned

- For lists, if the first element evaluates to a *function*, that function is
  applied to the rest of the values in the list

  - e.g., `(f x y z)` applies function `f` to the values `x`, `y`, and `z`

  - Arguments are passed *by value*; i.e., the argument sexps are evaluated 
    first, then their results are passed to the function

- The first element of a list may also be a *special form*, which is applied 
  like a function to its arguments, but with special semantics
-----------------------------------------------------------------------------|#

;; try evaluating some sexps

+

(+ 1 2)
(+ 1 2 3 4)

add1
(add1 5)

(+ 5 (add1 10))

(println "hello world")

(if (< (random 10) 5)
    (println "heads")
    (println "tails"))

; if is not a function, its a special form

#|-----------------------------------------------------------------------------
;; Quoting

The special form `quote` can be used to prevent the normal evaluation of a 
sexp, and instead just return the value of the sexp.

`quote` also has the short form `'` (i.e., syntactic sugar):

  (quote x) == 'x
  (quote (1 2 3)) == '(1 2 3)

There is also another special form, `quasiquote`, which can be used with 
`unquote` to selectively build sexps with some evaluated sub-sexps.

  (quasiquote x) == `x
  (quasiquote (x (unquote y) z)) == `(x ,y z)

Quasiquoting is particularly useful for metaprogramming!
-----------------------------------------------------------------------------|#

;; try evaluating some quote/quasiquote/unquote-based forms

(quote (+ 1 2))
'(+ 1 2 (...))

(quasiquote (+ 1 2))
(quasiquote (+ 5 (+ 1 2)))
(quasiquote (+ 5 (unquote (+ 1 2))))
; or
`(+ 5 ,(+ 1 2))

`(+ 5
    ,(add1 5) ; this runs
    (* 3
       ,(+ 4 8))) ; this runs


#|-----------------------------------------------------------------------------
;; Variables

Define global variables with `define` and local variables with `let` and `let*`
-----------------------------------------------------------------------------|#

(define *course-id* "CS 440")  ; sometimes we use "earmuffs" for global vars
                               ; "earmuffs" = * *

(define bignum (expt 2 50))

;; introducing local vars
; local via let
(define cnum (let ([x 10]
                   [y 44])
               (* x y)))
; cnum = 440
; let x = 10 and y = 44
; cnum = x * y
; local to within the parenthesis around the let

;; find roots of x^2 + 3x - 4 = (x - 1)(x + 4) = 0
;;  - need let* to use earlier vars when defining later ones
(define roots (let* ([a  1]
                     [b  3]
                     [c -4]
                     [disc (- (* b b) (* 4 a c))] ; this line will error is just let not let*
                     [sqr-disc (sqrt disc)])
                `(,(/ (+ (- b) sqr-disc) (* 2 a))
                  ,(/ (- (- b) sqr-disc) (* 2 a)))))

roots

#|-----------------------------------------------------------------------------
;; Pairs and Lists

Programmatically, lists are built out of linked pairs, where a pair is 
constructed using the `cons` function:

  (cons x y)

The functions `car` and `cdr` access the first and second slots of a pair.

  (car (cons x y)) => x
  (cdr (cons x y)) => y

A list is either:

- empty (expressed as `null`, `empty`, or `'()`), or
- a pair whose `car` refers to an element and whose `cdr` to a list

Useful functions:
  - `cons`: constructs a pair from an element and a list
  - `car`:  returns the first element of a pair
  - `cdr`:  returns the rest of a pair
  - `pair?`: tests whether an object is a pair
  - `list`: constructs a list from a sequence of elements
  - `first`: returns the first element of a non-empty list
  - `rest`:  returns the rest of a non-empty list  
  - `list?`: tests whether an object is a list
  - `empty?`: tests whether a list is empty
-----------------------------------------------------------------------------|#

;; pairs aren't necessarily lists!

(define pair1 (cons 1 2))

(define pair2 (cons 3 pair1))

(define pair3 '(1 . 2)) ; `.` indicates that the next value is the cdr of a pair

(define pair4 '(3 1 . 2)) ; = (cons 3 (cons 1 2))

(println "pairs examples:")
pair1
pair2
pair3
pair4

;; build and take apart some lists

(define lst1 '())

(define lst2 (cons 1 '()))

(define lst3 (cons 1 (cons 2 (cons 3 '()))))

(define lst4 (cons 1 (cons "hello" (cons #t '()))))
(car (cdr (cdr lst4)))
; or
(caddr lst4)

(define lst5 (list 1 "hello" #t))

(define lst6 '(1 "hello" #t))

(define lst7 '(1 "hello" #t . ())) ; lst6 and lst7 same, #t . () is just the last cons #t '()

#; (define lst8 (list 1 (2 3) ((4 5) (6 7)))) ; what's wrong with this?
; try to do function application but 2, 4, 6 not functions

; list is a function so it will eval insides were as quote wont

(define lst9 '(1 (2 3) ((4 5) (6 7))))

(define lst10 '(a (b (c d) (e f)) g))



#|-----------------------------------------------------------------------------
;; User defined types with `struct`
-----------------------------------------------------------------------------|#

;; define a `widget` type
(struct widget          ; type name
  (name purpose price)  ; attributes
  #:transparent)        ; when printing a widget, show its attributes

;; we get the following functions for free:
;; - `widget`: constructor
;; - `widget?`: predicate that returns #t for widget values
;; - `widget-name`: retrieve `name` attribute
;; - `widget-purpose`: retrieve `purpose` attribute
;; - `widget-price`: retrieve `price` attribute

(define w1 (widget "wrench" "wrenching" 9.99))
(define w2 (widget "plier" "pliering" 12.99))


;; define a `doohickey` type that is a sub-type of `widget`
(struct doohickey widget (special-power) #:transparent)

(define d1 (doohickey "thingamajig" "thinging" 199.99 "time travel"))


;; define my own cons
(struct mycons (car cdr) #:transparent)

;; build and work with some of our lists ...