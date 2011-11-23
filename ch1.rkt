#lang racket
(define (square x) 
  (* x x))
(define (abs x)
  (cond ((> x 0) x)
        ((< x 0) (- x))
        ((= x 0) 0)))

(define (ifabs x)
  (if (< x 0)
      (- x)
      x))


(define (sqrt-iter guess x)
  
  (define (good-enough? guess)
  (if (< (abs (- x (square guess))) 0.00001)
      #t
      #f))

(define (improve guess)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2.0)) 
  (if (good-enough? guess)
      guess
      (sqrt-iter( improve guess) x)))

(define (fib n)
  (cond ((< n 2) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-iter a b count)
  (cond ((= count 0) b)
        (else (fib-iter (+ a b) a (- count 1)))))

(define (close-enuf? old new)
  (define tolerance 0.00001)
  (if (< (abs (- new old)) tolerance)
      #t
      #f))

; Square Root problem 
; We are looking for a fixed point of the function x.
(define (fixed-point f start)
  ; Internal loops are defined using internal procedures
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (average x y)
  (/ (+ x y) 2.0))

(define  average-damp  
  ;NOTE : average-damp is the name given to a procedure (hence no parens) ,
  ; That takes as its input another procedure f 
  ; and returns a procedure 
  ;     that would take an x and return the average of x and f(x).
  (λ (f) 
    (λ (x) (average (f x) x))))

;;;; The following is the awesomest program I have ever seen! 
;;;; Square Root by Newton's method.
(define (sqrt-newton  x)
  ;; Wishful thinking : Assuming that newtons method exists
  (newton (λ(y) (- x (square y))) 1.0))

(define (newton f guess)
  ;; Wishful thinking again. Assume function to return a derivative of a function exists
  (define f-dash (deriv f))
  (fixed-point 
   (λ(x) (- x (/ (f x) (f-dash x))))
   guess))

(define dx 0.00000001)

(define deriv 
  (λ (f) ;Input
    ;Returns the derivative function
    (λ (x) 
      (/ (- (f (+ x dx))
            (f x)) 
         dx))))

;;;; Rights and Priveleges of First Class Citizens in Programming Lang
; To be named by variables
; To be passed as arguments to procedures
; To be returned as values of procedures
; To be incorporated into data structures
    
  ;;;;;;;;;;;;; Exercises ;;;;;;;;;;;;
;;;;;;;; 1.29 ;;;;;;;;;;; 
(define (cube x)
  (* x x x ))
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define y 
    (λ(k)
      (f (+ a (* k h)))))
  (* (/ h 3) (cond ((= n 0) (y 0))
                   (else (+ (* 2 (y n) )
                            (* 4 (y (- n 1)))
                            (simpson-integral f a b (- n 2)))))))
                          
  ;;;;; 1.30
(define (identity x) x)
(define (inc x) (+ x 1 ))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-iter term a next b)
  ;; Always iteration is implemented by an inner function. with an accumulator
  (define (iter a result)
    (if (> a  b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (sum-int a b)
  (sum identity a inc b))
(define (sum-int-iter a b)
  (sum-iter identity a inc b))

;;;;;;;;; 1.31 
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-int a b)
  (product identity a inc b))

(define (product-int-iter a b)
  (product-iter identity a inc b))

(define (factorial n)
  (product-int-iter 1 n))

(define (approximate-pi)
  (define (term x) (/ (* x  (+ x 2)) (square (+ x 1))))
  (define (next x) (+ x 2))
  (* 4 (product-iter term 2.0 next 100000000)))
;; Wow! Wonderful!

;;;;;;;;;; 1.32
;; Iterative Accumulator
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if ( > a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
;; Recursive Accumulator
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product-int-iter-using-accumulate a b)
  (accumulate * 1 identity a inc b))
