#lang racket
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))


(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) 
         (sum f (next a) next b))))

(define (cube x)
  (* x x x))
(define (sum-of-cubes a b)
  (define (cube x)
    (* x x x))
  (define (inc x)
    (+ x 1))
  (sum cube a inc b))


(define (integral f a b dx)
  (define (term x)
    (+ x (/ dx 2)))
  (define (next x)
    (+ x dx))
  (* (sum f (term a) next b ) dx))

