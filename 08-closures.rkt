#lang racket

#|-----------------------------------------------------------------------------
;; Adding Functions & Closures

We will add functions (as lambda expressions) and function applications to our
language. Our functions will have exactly one formal parameter each.

E.g.,

- lambda definition: `(lambda (x) (+ x 1))`

- function application: `((lambda (x) (+ x 1)) 10)`   (f x)

Though our language will not support named functions a la Racket's `define`,
we can use `let` to bind identifiers to lambdas. E.g.,

  (let ([f (lambda (x) (+ x 1))])
    (f 10))
-----------------------------------------------------------------------------|#

;; Some test cases (what should they evaluate to?)
(define p1 '(lambda (x) (+ x 1)))

(define p2 '((lambda (x) (+ x 1)) 10))

(define p3 '(let ([f (lambda (x) (+ x 1))])
              (f 10)))

;; p4-p5 for testing strict/lazy eval
(define p4 '(let ([x (+ 1 2)])
              20))

(define p5 '(let ([f (lambda (x) 10)])
              (f (+ 1 2))))

;; p6-p9 for testing closures (current not closures so do diff from racket)
(define p6 '(let ([x 10])
              (lambda (y) (+ x y))))

(define p7 '(let ([x 10])
              ((lambda (y) (+ x y)) 20)))

(define p8 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (let ([x 20])
                (f x)))) ; should be 30 but ours 40

(define p9 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (f 20))) ; fail dynamic binding


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

(struct closure (id body env) #:transparent)

;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expressions
    [(list '+ lhs rhs)
     (arith-exp "PLUS" (parse lhs) (parse rhs))] 
    [(list '* lhs rhs)
     (arith-exp "TIMES" (parse lhs) (parse rhs))]
    
    ;; identifiers (variables)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]
    
    ;; lambda expressions
    [(list 'lambda (list id) body)
     (lambda-exp id (parse body))]

    ;; function application
    [(list fn arg)
     (app-exp (parse fn) (parse arg))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Interpreter
(define (eval-s expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]

      ;; arithmetic expressions    
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         

      ;; variable binding
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair
             (cdr pair)
             (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v)
                               (eval-env v env))
                             val))])
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       expr]
      
      ;; function application

      ; ((lambda (x) (+ x 1)) (* 2 5))
      ; ((lambda (x) (+ x 1)) (10))
      ; (+ x 1) where x=10
      [(app-exp fn arg) 
       (match-let ([(lambda-exp id body) (eval-env fn env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg-val) env)))]

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))

(define (eval-l expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]

      ;; arithmetic expressions    
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         

      ;; variable binding
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair
             (eval-env (cdr pair) env) ; eval var when encountered
             (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id val)]) ; removed map to be lazy eval, dont eval the vals
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       expr]
      
      ;; function application

      ; ((lambda (x) (+ x 1)) (* 2 5))
      ; ((lambda (x) (+ x 1)) (10))
      ; (+ x 1) where x=10
      [(app-exp fn arg) 
       (match-let ([(lambda-exp id body) (eval-env fn env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg) env)))] ; changed arg-val to just arg

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))

(define (eval-lex expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]

      ;; arithmetic expressions    
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         

      ;; variable binding
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair
             (cdr pair)
             (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v)
                               (eval-env v env))
                             val))])
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       ; when eval lambda, save current env in closure
       (closure id body env)]
      
      ;; function application

      ; ((lambda (x) (+ x 1)) (* 2 5))
      ; ((lambda (x) (+ x 1)) (10))
      ; (+ x 1) where x=10
      [(app-exp fn arg) 
       (match-let ([(closure id body clenv) (eval-env fn env)]
                   [arg-val (eval-env arg env)])
         ; eval fn body in closure env
         (eval-env body (cons (cons id arg-val) clenv)))]

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))

;; REPL
(define (repl)
  (let ([stx (parse (read))])
    (when stx
      (println (eval stx))
      (repl))))