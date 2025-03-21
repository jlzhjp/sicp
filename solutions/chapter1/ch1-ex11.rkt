#lang racket/base

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(module+ test
  (require rackunit)

  (test-case
   "test recursive f"
   (check-= (f 0) 0 0)
   (check-= (f 1) 1 0)
   (check-= (f 2) 2 0)
   (check-= (f 3) 4 0)
   (check-= (f 4) 11 0)
   (check-= (f 5) 25 0)
   (check-= (f 6) 59 0)))


(define (f* n)
  (define (f-iter fn-1 fn-2 fn-3 n-cur)
    (let ([fn (+ fn-1 (* 2 fn-2) (* 3 fn-3))])
      (if (= n-cur n)
          fn
          (f-iter fn fn-1 fn-2 (+ n-cur 1)))))
  (if (< n 3)
      n
      (f-iter (f* 2) (f* 1) (f* 0) 3)))

(module+ test
  (test-case
   "test iterative f"
   (check-= (f* 0) 0 0)
   (check-= (f* 1) 1 0)
   (check-= (f* 2) 2 0)
   (check-= (f* 3) 4 0)
   (check-= (f* 4) 11 0)
   (check-= (f* 5) 25 0)
   (check-= (f* 6) 59 0)))