#lang planet neil/sicp
 

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
             "Insufficient funds"))
;;; Ex 3.1
(define (make-accumulator amount)
  (let ((balance amount)) ; initialize with amount
    (lambda (amount)
      (begin (set! balance (+ balance amount))
             balance))))

;;; Ex 3.2
(define (make-monitored f)
  (let ((counter 0))
    (lambda(x)
      
      (cond ((eq? x 'how-many-calls) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (f x)))))))
(define (square x) (* x x))

(define (contains? l item)
  (cond ((eq? l '()) #f)
        ((eq? (car l) item) #t)
        (else (contains? (cdr l) item))))


;;; Ex 3.3 and 3.4
(define (make-account balance password)
  (let ((num-error 0)
        (password-list (list password)))
     
    (define (call-the-cops)
      (error "Incorrect Password too many times... Cops are called"))
    
    (define (password-correct? pwd)
            (contains? password-list pwd))
    
    (define (set-new-password new-pwd)
      (set! password-list (append password-list (list new-pwd))))
    
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    
  
    (define (dispatch pwd m)
      (cond ((not (password-correct? pwd))
           (begin (set! num-error (+ num-error 1))
           (cond ((> num-error 7) (call-the-cops))
                 (else (error "Incorrect Password" num-error)))))
          (else (cond ((eq? m 'withdraw) withdraw)
                      ((eq? m 'deposit) deposit)
                      ((eq? m 'check-password) password-correct?)
                      ((eq? m 'set-new-password) set-new-password) 
                      (else (error "Unknown request -- MAKE-ACCOUNT"
                                   m))))))
  dispatch))

;;; Ex 3.7
(define (make-joint original-account original-password new-password)
  (cond ((original-account original-password 'check-password) ((original-account original-password 'set-new-password) new-password) original-account)
        (else (error "Cant create joint account! Wrong password"))))
;;; Ex 3.8
(define f
  (let ((state 0))
    (define (switch-state x)
      (let ((old-state state))
        (set! state (+ x state))
        old-state))
    switch-state))


;; 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y) x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
;;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let (( temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;; 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;(define at1 '(a b c)) ;; Returns 3
;(define x '(a))
;(define at2 (list x x)) ;;; Returns 4
;(define y (cons x x))
;(define at3 (cons y y))

;;; TODO: 3.17, 3.18, 3.19

;;; 3.20
; Done in book

;;; Queue Implementation
(define (make-queue)
  (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "Empty Queue"))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))
(define (print-queue queue)
  (cond ((empty-queue? queue)'())
        (else (display (car queue))
              (print-queue (cdr queue)))))
  


