#lang racket/base

(require support)

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (sicp-random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0) (/ trials-passed trials)]
          [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
          [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define ratio
    (monte-carlo
     trials
     (lambda ()
       (let ([x (random-in-range x1 x2)]
             [y (random-in-range y1 y2)])
         (p x y)))))
  (* (- x2 x1) (- y2 y1) ratio))

(module+ main
  (- (estimate-integral
      (lambda (x y)
        (<= (+ (square (- x 5)) (square (- y 7))) 9))
      2.0
      8.0
      4.0
      10.0
      1000000)
     (* 3.1415926535 (square 3))))
