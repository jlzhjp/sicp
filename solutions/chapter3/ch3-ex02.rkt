#lang racket/base

(define (make-monitored f)
  (let ([counter 0])
    (lambda args
      (if (eq? (car args) 'how-many-calls)
          counter
          (begin (set! counter (+ counter 1))
                 (apply f args))))))

(module+ test
  (require akari-sicp/lib/testing)

  (define s (make-monitored sqrt))

  (check-= (s 100) 10 0)
  (check-= (s 'how-many-calls) 1 0))
