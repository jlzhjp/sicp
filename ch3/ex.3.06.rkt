#lang racket/base

(define (rand-update x)
  (let ([m (- (expt 2 31) 1)]
        [a 48271]
        [c 0])
    (remainder (+ (* a x) c) m)))

(define (make-rand initial)
  (let ([x initial])
    (define (rand m . args)
      (cond [(eq? m 'generate) (begin (set! x (rand-update x)) x)]
            [(eq? m 'reset) (set! x (car args))]
            [else (error "Unknown request -- RAND" m)]))
    rand))

(module+ test
  (require sicp-lib/testing)

  (define rand (make-rand 1))

  (check-= (rand 'generate) 48271 0)
  (check-= (rand 'generate) 182605794 0)
  (check-= (rand 'generate) 1291394886 0)

  (rand 'reset 1)

  (check-= (rand 'generate) 48271 0)
  (check-= (rand 'generate) 182605794 0)
  (check-= (rand 'generate) 1291394886 0))