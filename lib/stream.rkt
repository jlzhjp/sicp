#lang racket/base

(provide cons-stream
         collect-stream
         the-empty-stream
         stream-null?
         stream-car
         stream-cdr
         stream-ref
         stream-for-each
         display-stream
         stream-memo?
         stream-enumerate-interval
         stream-filter
         delay
         force)

(define stream-memo? (make-parameter #t))

(define (memo-proc proc)
  (let ([already-run? #f]
        [result #f])
    (lambda ()
      (if (or (not already-run?) (not (stream-memo?)))
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define-syntax-rule (delay exp)
  (memo-proc (lambda () exp)))

(define (force delayed-object) (delayed-object))

(define-syntax-rule (cons-stream x y) (cons x (delay y)))

(define stream-null? null?)

(define the-empty-stream '())

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream)))]
        [else (stream-filter pred (stream-cdr stream))]))

(define (collect-stream s n)
  (if (or (stream-null? s) (= n 0))
      '()
      (cons (stream-car s) (collect-stream (stream-cdr s) (- n 1)))))
