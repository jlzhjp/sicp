#lang racket/base

(require racket/list)
(require (only-in "ch2-ex07.rkt"
                  make-interval
                  upper-bound
                  lower-bound)
         (rename-in "ch2-ex09.rkt"
                    [mul-interval old-mul-interval]))

(define (mul-interval x y)
  (let ([lx (lower-bound x)]
        [ux (upper-bound x)]
        [ly (lower-bound y)]
        [uy (upper-bound y)])
    (cond [(and (>= lx 0) (>= ly 0)) (make-interval (* lx ly) (* ux uy))]
          ; 翻译错误：
          ; 译文：每种情况中所需的乘法都不超过两次
          ; 原文：only one of which requires more than two multiplications.
          [(and (< lx 0 ux) (< ly 0 uy)) (make-interval (min (* lx uy) (* ux ly))
                                                        (max (* lx ly) (* ux uy)))]
          [(and (<= ux 0) (<= uy 0)) (make-interval (* ux uy) (* lx ly))]
          [(and (>= lx 0) (< ly 0 uy)) (make-interval (* ux ly) (* ux uy))]
          [(and (>= lx 0) (<= uy 0)) (make-interval (* ux ly) (* lx uy))]
          [(and (< lx 0 ux) (<= uy 0)) (make-interval (* ux ly) (* lx ly))]
          [else (mul-interval y x)]))) ; other 3 cases


(module+ test
  (require rackunit)

  (define boundaries '(-3 -2 -1 0 1 2 3))

  (define intervals (map (lambda (p) (make-interval (car p) (cadr p)))
                         (filter (lambda (p) (< (car p) (cadr p)))
                                 (cartesian-product boundaries boundaries))))
  (define testcases (cartesian-product intervals intervals))

  (for ([testcase testcases])
    (let ([interval1 (car testcase)]
          [interval2 (cadr testcase)])
      (with-check-info
          (['interval1 interval1]
           ['interval2 interval2])
        (check-equal? (mul-interval interval1 interval2)
                      (old-mul-interval interval1 interval2))))))