#lang racket/base

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd m)
    (cond [(not (eqv? pwd password)) (lambda _ "Incorrect password")]
          [(eqv? m 'withdraw) withdraw]
          [(eqv? m 'deposit) deposit]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))

  dispatch)

(module+ test
  (require support/testing)

  (define acc (make-account 100 'secret-password))

  (check-= ((acc 'secret-password 'withdraw) 40) 60 0)
  (check-equal? ((acc 'some-other-password 'deposit) 50)
                "Incorrect password"))
