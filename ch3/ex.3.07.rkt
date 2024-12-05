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
    (cond [(not (eq? pwd password)) (lambda _ "Incorrect password")]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))

  dispatch)

(define (make-joint acc password new-password)
  (lambda (pwd m)
    (if (not (eq? pwd new-password))
        (lambda _ "Incorrect password")
        (acc password m))))

(module+ test
  (require support/testing)

  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rose-bud))

  (check-= ((peter-acc 'open-sesame 'deposit) 20) 120 0)
  (check-= ((paul-acc 'rose-bud 'withdraw) 10) 110 0)
  (check-equal? ((paul-acc 'open-sesame 'withdraw) 10) "Incorrect password"))
