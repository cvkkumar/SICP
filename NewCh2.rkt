#lang racket
(define (power x y)
  (define (iter p x y)
    (if (= y 0) p
        (iter (* p x) x (- y 1))))
  (iter 1 x y))

(define (gcd x y)
  (if (= y 0) x
      (gcd y (remainder x y))))

(define (make-rat numer denom)
  (cons numer denom))

(define (numer x)
  (let ((sign (signum (* (car x) (cdr x))))
        (g (gcd (car x) (cdr x))))
    (* sign (abs (/ (car x) g)))))
  

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (abs (/ (cdr x) g))))
  

(define (signum x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

(define (negative? rat) 
  (= (signum (* (numer rat)
                (denom rat)))
     -1))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))               
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat                
            (* (denom x) (denom y))
            (* (numer x) (numer y))))


;;; Ex 2.2

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  "p1 and p2 are points"
  (cons p1 p2))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (midpoint-segment l)
  (let ((p1 (start-segment l))
        (p2 (end-segment l)))
    (make-segment (/ (+ (x-point p1)
                        (x-point p2))
                     2.0)
                  (/ (+ (y-point p1)
                        (y-point p2))
                     2.0))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment l)
  (newline)
  (print-point (start-segment l))
  (display "->")
  (print-point (end-segment l)))
  
;;; Ex 2.5
(define (cons-my-pair x y)
  (* (power 2 x) (power 3 y)))
(define (repeated-div n d)
  (define (iter result n d)
    (if (> (remainder n d) 0) result
        (iter (+ result 1) (/ n d) d)))
  (iter 0 n d))

(define (car-my-pair z)
  (repeated-div z 2))
(define (cdr-my-pair z)
  (repeated-div z 3))


;;; Ex 2.7 - INTERVAL ARITHMETIC

(define (make-interval a b)
  (cons a b))

(define (upper-bound z)
  (cdr z))
(define (lower-bound z)
  (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) ( upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) ( upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (interval-spans-zero? x)
  (and (> (upper-bound x) 0) 
       (< (lower-bound x) 0)))

(define (div-interval x y)
  (mul-interval x 
                (if (interval-spans-zero? y) (error "Interval Spans zero" ) 
                    (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))


(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x))
     2.0))

        