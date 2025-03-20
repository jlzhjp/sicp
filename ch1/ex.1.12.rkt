#lang racket/base

(require racket/string)

(define (pascal-triangle-item r c)
  (cond [(= r c 1) 1]
        [(or (< r 1) (< c 1)(> c r)) 0]
        [else (+ (pascal-triangle-item (- r 1) c)
                 (pascal-triangle-item (- r 1) (- c 1)))]))

(define (print-pascal-triangle n)
  (define (print-pascal-triangle-row r)
    (define (print-pascal-triangle-item c)
      (when (<= c r)
        (display (pascal-triangle-item r c))
        (display " ")
        (print-pascal-triangle-item (+ c 1))))
    (when (<= r n)
      (print-pascal-triangle-item 1)
      (newline)
      (print-pascal-triangle-row (+ r 1))))
  (print-pascal-triangle-row 1))

(module+ test
  (require sicp-lib/testing)

  (check-normalized-output
   (lambda () (print-pascal-triangle 5))

   '("1"
     "1 1"
     "1 2 1"
     "1 3 3 1"
     "1 4 6 4 1")))
