#lang racket

(require racket/trace)

#|
MP2 help???


|#

#|-----------------------------------------------------------------------------
;; Prelude: user defined types with `struct`
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

;; we can also match against structs
(define (which-widget? w)
  (match w
    [(widget "wrench" _ _) "It's a wrench!"]
    [(doohickey "thingamajig" _ _ _) "It's a thingamajig!"]
    [(? widget?) "It's some sort of widget"]
    [_ "I don't know what this is"]))

(which-widget? w1)
(which-widget? d1)
(which-widget? w2)
(which-widget? 42)

#|-----------------------------------------------------------------------------
;; Our language

We're going to start with a very simple language and slowly add to it. Our
first iteration will support integer literals, the binary arithmetic operations
 +` and `*`, and `let`-bound variables. The syntax will mirror Racket's. 
-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 "(+ 1 2)")

(define p2 "(* 2 (+ 3 4))")

(define p3 "(+ x 1)")

(define p4 "(* w (+ x y))")

(define p5 "(let ([x 10])
              (+ x 1))")

(define p6 "(let ([x 10]
                  [y 20])
              (+ x y))")

(define p7 "(let ([x 10])
              (let ([y 20])
                (+ x (let ([w 30])
                       (* w y)))))")



#|-----------------------------------------------------------------------------
;; Parser

Review: What is parsing?

- Input string (source language) => Syntax object

- A syntax object contains information about the structure of the code. Often,
  we use a *tree* as an underlying representation.

  - E.g., the code "(let ([x 10]) (+ x 1))"
          
          may be parsed to the syntax tree:

                    let
                   /   \
                  x     +
                 /     / \
                10    x   1

  - Racket sexps are a programmatic way of representing syntax trees!

- Since our syntax mirrors Racket's, we may rely on Racket's reader to parse
  our input to produce an initial syntax tree.
-----------------------------------------------------------------------------|#

;;; using Racket's reader
#; (read)



#|-----------------------------------------------------------------------------
;; Parser (continued)

- Our syntax tree doesn't currently contain much information besides tokens
  pulled directly from the input string.

- Next, we will recursively descend through the syntax tree, "decorating" its
  nodes with information that can help us expand and evaluate it.
-----------------------------------------------------------------------------|#

;;; Some types for decorating our syntax tree

;; integer value
(struct int-exp () #:transparent)

;; arithmetic expression
(struct arith-exp () #:transparent)

;; variable
(struct var-exp () #:transparent)

;; let expression
(struct let-exp () #:transparent)


;; Parser
(define (parse sexp)
  (void))


#|-----------------------------------------------------------------------------
;; Interpreter

- The interpreter's job is the take the (decorated) syntax tree and evalute it!
-----------------------------------------------------------------------------|#

;; Interpreter
(define (eval expr)
  (void))