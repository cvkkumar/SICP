#lang racket
(define (power x y)
  (define (iter p x y)
    (if (= y 0) p
        (iter (* p x) x (- y 1))))
  (iter 1 x y))


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

(define (-ve? x ) (< x 0))
(define (+ve? x) (or (= x 0) (> x 0)))


(define (fast-mul-interval x y)
  (let ((l1 (lower-bound x))
        (u1 (upper-bound x))
        (l2 (lower-bound y))
        (u2 (upper-bound y)))
    (cond ((and (and (-ve? l1) (-ve? u1))
                (and (-ve? l2) (-ve? u2))) (make-interval (* u1 u2) (* l1 l2)))
          ((and (and (+ve? l1) (+ve? u1))
                (and (+ve? l2) (+ve? u2))) (make-interval (* l1 l2) (* u1 u2)))
          
          ((and (and (-ve? l1) (-ve? u1))
                (and (+ve? l2) (+ve? u2)) (make-interval (* l1 u2) (* u1 l2))))
          ((and (and (+ve? l1) (+ve? u1))
                (and (-ve? l2) (-ve? u2)) (make-interval (* l2 u1) (* u2 l1))))
          ((and (and (-ve? l1) (-ve? u1))
                (and (-ve? l2) (+ve? u2)) (make-interval (* l1 u2) (* l1  l2))))
          ((and (and (-ve? l2) (-ve? u2))
                (and (-ve? l1) (+ve? u1)) (make-interval (* l2 u1) (* l2  l1))))
          
          ((and (and (+ve? l1) (+ve? u1))
                (and (-ve? l2) (+ve? u2)) (make-interval (* l2 u1) (* u2  u1))))
          
          ((and (and (+ve? l2) (+ve? u2))
                (and (-ve? l1) (+ve? u1)) (make-interval (* l1 u2) (* u1  u2))))
          
          ((and (and (-ve? l2) (+ve? u2))
                (and (-ve? l1) (+ve? u1)) (let ((p1 (* l1 u1))
                                                (p2 (* l1 u2))
                                                (p3 (* l2 u1))
                                                (p4 (* l2 u2)))
                                            (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))))))




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

;;; Ex 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c percentage-tolerance)
  (let ((cpercent (* c (/ percentage-tolerance 100.0))))
    (make-interval (- c cpercent) (+ c cpercent))))

(define (percentage-tolerance i)
  (* (/ (width i) (center i)) 100))

;;; Ex 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Answer written in margin of the textbook for 2.14 and 2.15

;;; Ex 2.17
(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))

(define (append-mine l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))
;;; Ex 2.18
(define (reverse-mine l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (append (reverse (cdr l)) (list (car l))))))

;;; Ex 2.19


(define (filter predicate l)
  (if (null? l) l
      (if (predicate (car l)) (append (list (car l)) (filter predicate (cdr l)))
          (filter predicate (cdr l)))))
          

;;; Ex 2.20
(define (same-parity x1 . w)
  (define (is-parity-same? x2)
    (let ((z (+ (abs x1) (abs x2))))
      (even? z)))
  (filter is-parity-same? (cons x1 w))) 
  
(define (square x)
  (* x x))

(define (map-mine proc l)
  (if (null? l)
      '()
      (cons (proc (car l)) (map proc (cdr l)))))
(define nil '())

;;; Ex 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
          answer
        (iter (cdr things)
              (append answer (list (square (car things))) ))))
  (iter items nil))

;;; Ex 223
(define (for-each func l)
  (cond ((null? l) #t)
        (else (func (car l)) 
              (for-each func (cdr l)))))

  
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;;; 2.27

(define (deep-reverse l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        ((pair? l) (append (deep-reverse  (cdr l)) (deep-reverse (list (car l)))))
        (else (append (deep-reverse (cdr l)) (list (car l))))))

