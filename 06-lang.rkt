#lang racket

(require (for-syntax racket/base racket/list)
         racket/string)


#|-----------------------------------------------------------------------------
;; Modules

A Racket source file will typically evaluate to a single top-level `module`,
which encapsulates the definitions found in the file. The `#lang` line we've
been using is actually shorthand for a `module` declaration.
-----------------------------------------------------------------------------|#

;; E.g., a standalone module
(module
    foo ; module name
  racket ; evaluation / initial import

  (provide bar) ; export basically

  (define (bar)
    (println "hello from bar"))

  (define (baz)
    (println "hello from baz"))
  )

(require 'foo)

(bar)

#|-----------------------------------------------------------------------------
;; The Reader and Expander -> reader is like the scanner+parser

So what *really* happens when we load a source file into a Racket interpreter?

- The `#lang` line (which we previously always wrote as `#lang racket`) tells
  Racket where to find the language's **reader**.

- The reader's job is to read in code from the source file and convert it into
  syntax objects

- The top-level syntax object returned by the reader is a `module` which:

  - Contains all the sub-expressions read from the source file

  - Identifies the **expander** to which the forms will be passed

  - E.g., a file named "foo.rkt" containing:

       #lang racket
       exp ...

    becomes:

       (module foo racket
        exp ...)

- The expander's job is to figure out how to take the syntax objects produced
  by the reader and "expanding" them into forms that can be evaluated in Racket

  - Macros may be useful at this stage!

- The Reader and Expander give us everything we need to create new languages!
-----------------------------------------------------------------------------|#

;; Implement a reader & expander for the "program" in 06-lang-demo.rkt

#; (port->lines (open-input-file "06-lang-demo.rkt"))

(provide read-syntax)

(define (read-syntax path port)
  (define (non-comment? line)
    (and (non-empty-string? line)
         (not (string-prefix? line "--"))))

  (define (make-form line)
    (let ([lst (string-split line)])
      `(update ,(first lst) ,(string->number (second lst)))))
  
  (let* ([src-lines (filter non-comment?
                            (map string-trim (port->lines port)))])
    (datum->syntax #f
                  `(module demo "06-lang.rkt"
                     ,@(map make-form src-lines)
                     players))))

(provide update players)

(define players (make-hash)) ; mutable hashtable

(define (update name val)
  (hash-set! players
             name ; key is name
             (+ val ; add new val to
                (hash-ref players name
                          0 ; default val 0
                          ))))


(define iht(hash)) ; immutable
(hash-update iht "a" add1 0) ; key function initial

(make-hash)
(define ht (make-hash))
ht
ht
(hash-set! ht "a" "apple")
ht
(hash-ref ht "a" 0)
ht

#|-----------------------------------------------------------------------------
;; Interposition points

Additional macros give us "hooks" into different stages of the process of
reading, expanding, and evaluating programs in our new language. They include:

- #%module-begin: module wrapper
- #%top-interaction: REPL wrapper
- #%datum: datum wrapper
- #%app: function application wrapper
- #%top: top-level identifier wrapper

Since we didn't use them in this (simple) language, we simply export the
default versions provided by racket/base.
-----------------------------------------------------------------------------|#

;; export default interposition macros
(provide #%module-begin
         #%top-interaction
         #%datum
         #%app
         #%top)
