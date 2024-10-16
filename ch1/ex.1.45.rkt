#lang racket/base

(require "../common.rkt")
(require (only-in "ex.1.35.rkt" fixed-point))
(require (only-in "ex.1.43.rkt" repeated))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (convergent? f guess max-iter)
  (define tolerance 0.00001)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (convergent-iter guess n)
    (if (> n max-iter)
        #f
        (let ([next (f guess)])
          (if (close-enough? next guess)
              #t
              (convergent-iter next (+ n 1))))))
  (convergent-iter guess 0))

(define max-iteration 100)

(define (find-average-damp-times-for-nth-root n first-guess)
  (define test-base 10)
  (define f (lambda (y) (/ test-base (expt y (dec n)))))

  (define (iter average-damp-times)
    (if (convergent? ((repeated average-damp average-damp-times) f)
                     first-guess
                     max-iteration)
        (displayln (format "~ath root need ~a times average damp" n average-damp-times))
        (iter (inc average-damp-times))))

  (iter 1))

(module+ main
  (for ([n (in-range 2 36)])
    (find-average-damp-times-for-nth-root n 1.0)))

#|
2th root need 1 times average damp
3th root need 1 times average damp
4th root need 2 times average damp
5th root need 2 times average damp
6th root need 2 times average damp
7th root need 2 times average damp
8th root need 3 times average damp
9th root need 3 times average damp
10th root need 3 times average damp
11th root need 3 times average damp
12th root need 3 times average damp
13th root need 3 times average damp
14th root need 3 times average damp
15th root need 3 times average damp
16th root need 4 times average damp
17th root need 4 times average damp
18th root need 4 times average damp
19th root need 4 times average damp
20th root need 4 times average damp
21th root need 4 times average damp
22th root need 4 times average damp
23th root need 4 times average damp
24th root need 4 times average damp
25th root need 4 times average damp
26th root need 4 times average damp
27th root need 4 times average damp
28th root need 4 times average damp
29th root need 4 times average damp
30th root need 4 times average damp
31th root need 4 times average damp
32th root need 5 times average damp
33th root need 5 times average damp
34th root need 5 times average damp
35th root need 5 times average damp
|#

(define (root x n)
  (define average-damp-times (floor (log n 2)))
  (define f (lambda (y) (/ x (expt y (dec n)))))
  (fixed-point ((repeated average-damp average-damp-times) f)
               1.0))

(module+ test
  (require rackunit)

  (check-= (root 10 2) (expt 10 1/2) 1e-5)
  (check-= (root 10 3) (expt 10 1/3) 1e-5)
  (check-= (root 10 4) (expt 10 1/4) 1e-5)
  (check-= (root 10 5) (expt 10 1/5) 1e-5)
  (check-= (root 10 6) (expt 10 1/6) 1e-5))