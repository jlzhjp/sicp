#lang racket/base

(define (call-the-cops . _) (displayln "Police coming!"))

(define (make-account balance password)
  (define error-count 0)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd m)
    (if (not (eqv? pwd password))
        (begin (set! error-count (+ error-count 1))
               (if (>= error-count 7)
                   call-the-cops
                   (lambda _ "Incorrect password")))
        (begin (set! error-count 0)
               (cond [(eqv? m 'withdraw) withdraw]
                     [(eqv? m 'deposit) deposit]
                     [else (error "Unknown request -- MAKE-ACCOUNT")]))))

  dispatch)

(module+ test
  (require sicp-lib/testing)

  (define acc (make-account 100 'secret-password))

  (check-equal? ((acc 'some-other-password 'deposit) 50) "Incorrect password")
  (check-= ((acc 'secret-password 'withdraw) 40) 60 0)
  (check-normalized-output
   (lambda ()
     (for ([_ (in-range 7)])
       ((acc 'some-other-password 'deposit) 50)))

   '("Police coming!")))
