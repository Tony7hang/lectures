#lang racket

(require macro-debugger/stepper)
(require (for-syntax racket/list))

;; syntax objects attach syntactical context to sexps

(define s1 (syntax "hello world"))

(define s2 (syntax (foo bar 1 2 3))) ; special form, wont eval first

(define s3 #'(foo bar 1 2 3)) ; #' is syntactic sugar for `syntax`

(define s4 #'(if (< 1 2) "true" "false"))

(define s5 #`(if #,(< 1 2) "true" "false")) ; kinda like the quasi quote

#; (values
    (syntax-source s3)
    (syntax-line s3)
    (syntax-column s3)
    (syntax->datum s3))

;; `eval-syntax` evaluates syntax objects, like `eval` for sexps

; (eval-syntax s4)


;; `datum->syntax` lets us create syntax objects from sexps (and context)

#; (datum->syntax #f '(println "hello world"))

#| -----------------------------------------------------------------------------
;; `define-syntax` defines a syntax transformer, aka macro ***** <<====
----------------------------------------------------------------------------- |#

(define-syntax always-say-hi
  (lambda (stx) ; stx ref to original stx obj
    #`(quote (hi #, stx))))

(expand-once #'(say-hi 1 2 3))

(define-syntax quote-myself void)

(define-syntax (infix stx)
  (let ([infix-exp (second (syntax->datum stx))])
    #`(#,(second infix-exp) #,(first infix-exp) #,(third infix-exp))))

(infix (2 * 4))

;; we can use `expand/step` (and others) to help us see the expansion of a macro

#; (expand/step #'(quote-myself ha ha ha))


(define-syntax (reversed stx) void)

#; (expand/step #'(reversed 1 2 3 +))

(define-syntax (my-if stx) void)


;; `syntax-case` provides us with pattern matching and takes a template
(define-syntax (my-if-2 stx)
  (syntax-case stx ()
    [(_ test exp1 exp2)  #'(cond [test exp1]
                                 [else exp2])]))


;; `define-syntax-rule` lets use more easily define syntax transformers
(define-syntax-rule (my-if-3 test exp1 exp2) void)

;; define a macro that implements a loop
(define-syntax-rule (loop n body)
  (let rec ([i 0])
    (when (< i n)
      body
      (rec (add1 i)))))
; can't do println i -> i undefined, can be seen from outside?

(loop 10 (println "hello"))
(expand-once #'(loop 10 (println "hello")))

;; define a macro that implements a for loop
(define-syntax-rule (for-loop var n body)
  (let rec ([var 0])
    (when (< var n)
      body
      (rec (add1 var)))))

(for-loop x 10 (println x))

#|------------------------------------------------------------------------------
;; Hygiene

- Racket macros are "hygienic" by design -- i.e., identifiers introduced by
  a macro exist in a separate lexical context from where it is called, and
  so cannot be accidentally (or intentionally) used by call-site code.
------------------------------------------------------------------------------|#

;; syntax objects created with `syntax` are hygienic -- their bindings are
;; determined by their lexical context (i.e., where they are defined):
(define x 440)

(define-syntax (hygienic stx)
  #'(println x))

#; (values
    (hygienic)
    (let ([x 10]) (hygienic)))

(expand-once #'(hygienic))

(hygienic) ; prints 440
(let ([x 10]) (hygienic)) ; prints 440, can't find x

(define-syntax (hygienic2 stx)
  #'(define foo 440)) ; foo not global, macro dont need to worry about conficts

; sometimes do want to break hygiene, ie make foo accessable/ may have conficts


;; but `datum->syntax` allows us to "break" hygiene by inheriting the lexical
;; context of some other syntax object (e.g., from the call site)
(define-syntax (unhygienic stx)
  (datum->syntax stx '(println x))) ; breaking hygiene

(unhygienic) ; prints 440
(let ([x 10]) (unhygienic)) ; prints 10, can find x

#; (values
    (unhygienic)
    (let ([x 10]) (unhygienic)))

(define-syntax (unhygienic2 stx)
  (datum->syntax stx '(define bar 440)))

(unhygienic2)
bar

#; (begin (unhygienic2)
       (println bar))


;; "Anaphoric if": a convenient programmer-defined control structure
;; 
;; an·a·phor | ˈanəˌfôr | (Noun)
;; - a word or phrase that refers to an earlier word or phrase
;;   (e.g., in "my cousin said she was coming", "she" is used as an
;;   anaphor for my cousin).

#; (aif (compute-test-result ...)  ; may be a lengthy computation
        (use it)      ; `it` refers to the result of the computation
        (else-case))  


;; what's wrong with the following attempt?
(define-syntax-rule (aif test exp1 exp2)
  (let ([it test])
    (if it
        exp1
        exp2)))


;; can you "fix" this? (need to manually break hygiene)
(define-syntax (aif-2 stx)
  (let ([sexp (syntax->datum stx)])
    (datum->syntax stx
    `(let ([it ,(second sexp)])
       (if it
           ,(third sexp)
           ,(fourth sexp))))))

(aif-2 (< 1 0)
       (println it)
       (println "nope"))


;; still not perfect!