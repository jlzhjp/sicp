#lang racket/base

(require ffi/unsafe/atomic)

(provide parallel-execute
         make-serializer
         make-mutex
         without-interrupts)

; https://stackoverflow.com/questions/13467753/
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (make-serializer)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ([cell (mcons #f '())])
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (when (test-and-set! cell)
               (the-mutex 'acquire))]
            [(eq? m 'release (clear! cell))]))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell #f))

(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         #t
         (begin (set-mcar! cell #t)
                #f)))))

(define without-interrupts call-as-atomic)
