#lang racket/base

(define (make-f)
  (let ([x -0.5])
    (lambda (y) (begin (set! x (+ x y))
                       x))))

(module+ test
  (require akari-sicp/lib/testing)

  (define f (make-f))
  (define arg1 (f 0))
  (define arg2 (f 1))

  (check-= (+ arg1 arg2) 0 0)

  (set! f (make-f))
  (set! arg2 (f 1))
  (set! arg1 (f 0))
  (check-= (+ arg1 arg2) 1 0))