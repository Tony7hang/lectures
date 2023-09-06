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
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; (trace fib)



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (void))


(define (repeat n x)
  (void))


(define (reverse lst)
  (void))


(define (range n)
  (void))