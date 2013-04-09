#lang racket
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
             "Insufficient funds"))
;;; Ex 3.1
(define (make-accumulator amount)
  (let ((balance amount)) ; initialize with amount
    (λ(amount)
      (begin (set! balance (+ balance amount))
             balance))))

;;; Ex 3.2
(define (make-monitored f)
  (let ((counter 0))
    (λ(x)
      
      (cond ((eq? x 'how-many-calls) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (f x)))))))
(define (square x) (* x x))

;;; Ex 3.3 and 3.4
(define (call-the-cops)
  (error "Incorrect Password too many times... Cops are called"))
(define (make-account balance password)
  (let ((num-error 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    
    (cond ((not (eq? pwd password))
           (begin (set! num-error (+ num-error 1))
           (cond ((> num-error 7) (call-the-cops))
                 (else (error "Incorrect Password" num-error)))))
          (else (cond ((eq? m 'withdraw) withdraw)
                      ((eq? m 'deposit) deposit)
                      (else (error "Unknown request -- MAKE-ACCOUNT"
                                   m))))))
  dispatch))