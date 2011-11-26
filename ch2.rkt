#lang scheme

;;;;;; 2.1 Introduction to Data Abstraction

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; Let establishes local names by setting up contexts
;; Syntax (let (( var ) (expr))
;;          (... all statements that use var))

(define (make-rat n d)
  (let ((common-div (gcd n d)))
        (cons (/ n common-div) (/ d common-div) )))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (+rat a b)
  (make-rat 
   (+ 
    (* (numer a) (denom b))
    (* (numer b) (denom a)))
   (* (denom a) (denom b))))

(define (*rat a b)
  (make-rat
   (* (numer a) (numer b))
   (* (denom a) (denom b))))

;;; The real power comes through when we use this kind of data abstraction as BUILDING blocks of much larger systems

(define (point x y)
  (cons x y))
(define (getx point)
  (car point))

(define (gety point)
  (cdr point))

(define (segment startpoint endpoint)
  (cons startpoint endpoint))
(define (start-seg segment)
  (car segment))
(define (end-seg segment)
  (cdr segment))

(define (square x) (* x x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



(define (length s)
  (let 
      ((dx (- (getx (end-seg s))
              (getx (start-seg s))))
       (dy (- (gety (end-seg s))
              (getx (start-seg s)))))
    (sqrt (+
           (square dx)
           (square dy)))))

    
    
;;; CLOSURE : means of combination where same

