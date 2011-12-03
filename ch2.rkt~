#lang scheme

;;;;;; 2.1 Introduction to Data Abstraction

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (and x-p y-p)
  (cond (x-p
         (cond (y-p #t)
               (else #f)))
        (else #f)))



;; Let establishes local names by setting up contexts
;; Syntax (let (( var ) (expr))
;;          (... all statements that use var))

(define (make-rat n d)
  (let ((nr (/ n (gcd n d)))
        (dr (/ d (gcd n d))))
    (cond ((< dr 0) ( cons (- nr) (- dr)))
          (else (cons nr dr)))))

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

(define (equal? a b)
  (and (= (numer a) (numer b)) (= (denom a) (denom b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; The real power comes through when we use this kind of data abstraction as BUILDING blocks of much larger systems
;;; Ex 2.2 

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (add-point p1 p2)
  (make-point (+ (x-point p1)
                 (x-point p2))
              (+ (y-point p1)
                 (y-point p2))))

(define (scale-point p scale)
  (make-point (* scale (x-point p))
              (* scale (y-point p))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Now point is defined.

(define (make-segment startpoint endpoint)
  (cons startpoint endpoint))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (scale-point 
   (add-point
    (start-segment segment)
    (end-segment segment))
   0.5))

(define (square x) (* x x))
;;;; WOW!!!
(define (my-cons x y)
  (λ(m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1- CONS" m)))))
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))
;;;; NOTE : this is possible because my-cons returns a function object.

;;; Ex 2.4
(define (your-cons x y)
  (lambda (m) (m x y)))
(define (your-car z)
  (z (lambda (p q) p)))

(define (your-cdr z)
  (z (lambda (p q) q)))

;;; Ex 2.5 - SOOOOPER !! :)
(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))
(define (my-func num result-2 result-3)
  (cond ((<= num 1) (λ(x) (cond ((= x 0) result-2)
                                (else result-3))))
        (else (cond ((= (remainder num 2) 0) (my-func (/ num 2.0) (+ result-2 1) result-3))
                    (else  (my-func (/ num 3) (+ result-2 0) (+ result-3 1)))))))
(define (car2 z)
  ((my-func z 0 0) 0))

(define (cdr2 z)
  ((my-func z 0 0) 1))


;;; Ex 2.6
;;;;2.6 : OUT of scope for me currently. I m mindblown! 

;;; Ex 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((= (width y) 0)
         (error "Div by 0:div-interval"))
        (else (mul-interval x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))

(define (make-interval a b) (cons a b))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

;;; Ex 2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y)))
        (p5 (- (lower-bound y) (lower-bound x)))
        (p6 (- (lower-bound y) (upper-bound x)))
        (p7 (- (upper-bound y) (lower-bound x)))
        (p8 (- (upper-bound y) (upper-bound x))))
    (make-interval (min p1 p2 p3 p4 p5 p6 p7 p8)
                   (max p1 p2 p3 p4 p5 p6 p7 p8)))
  )



;;;; TODO : Complete Alyssa HAcker problems

;; To get the nth element of the list
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length-recur items)
  (if (null? items)
      0
      (+ 1 (length-recur (cdr items)))))

(define (length items)
  (define (length-iter l count)
    (if (null? l)
        count
        (length-iter (cdr l) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Ex 2.17

(define (last-pair l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))))

;;; Ex 2.18
(define (reverse l)
  (cond ((null? l) '())
        ((null? (cdr l)) (list (car l)))
        (else (append (reverse (cdr l)) (list (car l))))))

;;; Ex 2.19

(define (count-change amount)
  (cc amount 5))

;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount (- kinds-of-coins 1))
;                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
;
;(define (first-denomination kinds-of-coins)
;  (cond ((= kinds-of-coins 1) 1)
;        ((= kinds-of-coins 2) 5)
;        ((= kinds-of-coins 3) 10)
;        ((= kinds-of-coins 4) 25)
;        ((= kinds-of-coins 5) 50)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount 
                (except-first-denomination coin-values))
            (cc (- amount 
                   (first-denomination coin-values))
                coin-values)))))
;;;; 2.19
(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

;;;; 2.20 : See, i used the dotted notation.
(define (same-parity x . l)
  (append (list x) 
          (filter (λ(y) (= (- (remainder x 2) (remainder y 2)) 0)) l)))

;;; 2.21
(define (square-list-by-map l)
  (map (λ(x) (square x)) l))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

;;; 2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer 
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items '()))

;; The answer is in reverse order because of cons. The correct implementation is above.

;;; 2.23

(define (for-each f items)
  (if (null? items)
      '()
      (append (list (f (car items))) (for-each f (cdr items))))
  #t)
  

(define (count-leaves item)
  (cond ((null? item)  0)
        ((not (pair? item)) 1)
        (else (+ 
               (count-leaves (car item))
               (count-leaves (cdr item))))))
;;; 2.24
;Answer = 4. Only 1,2 3 and 4 are leaf nodes

;;; 2.25
;(car (cdr (car (cdr (cdr y)))))
;(car (car y))
;(cadr (cadr (cadr (cadr (cadr (cadr y))))))

;;; 2.26
; (define x (list 1 2 3))
; (define y (list 4 5 6))
; (append x y) => (1 2 3 4 5 6)
;(cons x y) => ((1 2 3) 4 5 6)
; (list x y) => ((1 2 3) (4 5 6)

;;; 2.27 - Deep Reversing a list - WOW. I am not sure i completely understand how i wrote it!
(define (deep-reverse l)
  (cond ((null? l) '())
        ((null? (cdr l)) (cond ((list? (car l)) (list (deep-reverse (car l))))
                               (else (list (car l)))))
        (else (append (deep-reverse (cdr l)) (deep-reverse (list (car l)))))))


;;; 2.28 

(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l) )
        (else (append (fringe  (car l)) (fringe (cdr l))))))
        