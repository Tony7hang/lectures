#lang racket

(require racket/trace) ; for dynamic function call traces


#|-----------------------------------------------------------------------------
;; Recursion (and Iteration)
-----------------------------------------------------------------------------|#

(define (println-times datum n)
  (when (> n 0)
    (println datum) 
    (println-times datum (sub1 n))))


;; integer summation
(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (sub1 n)))))
; O(n) runtime complexity
; also O(n) space complexity -> each recursion req a new stack in stacks

; makes it show the steps ran whenever sum-to is called
(trace sum-to)

(trace-define (sum-to-acc n acc ) ; acc stands for "accumulator" tail recursion, go other way???
  (if (= n 0)
      acc
      (sum-to-acc (sub1 n) (+ n acc))))

; trace-define defines and also trace!
; Racket performs TCO "Tail-Call Optimization"

#| not working lol, just look in the completed notes for all the other ways to do this 
(define (sum-to-acc-2 n)
  (trace-define (sum i acc)
    (if (= n 0)
      acc
    (sum (sub1 n) (+ n acc))))
      (sum n 0))
|#


;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(trace-define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fib-tail n)
  (let rec ([f0 0]
            [f1 1]
            [i 0])
    (if (= i n)
        f0
        (rec f1 (+ f0 f1) (add1 i)))))



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (if (empty? lst)
      0
      (add1 (length (rest lst)))))


(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (sub1 n) x))))

(trace-define (concat l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (concat (rest l1) l2))]))

(define (reverse lst)
  (if (empty? lst)
      lst
      (concat (reverse (rest lst)) (list (first lst)))))


(define (rev-range n) ; (range 5) -> '(0 1 2 3 4) = (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
  (if (< n 0)
      '()
      (cons (sub1 n) (range (sub1 n))))) ; this prints backwards 4 3 2 1 0

(define (range n)
  (trace-let rec ([i 0]) ; kinda like a for loop, rec ~ for
    (if (= i n)
        '()
        (cons i (rec (add1 i))))))


