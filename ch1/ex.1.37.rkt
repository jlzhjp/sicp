#lang racket/base

(provide cont-frac
         cont-frac*)

(require "../common.rkt")

(define (cont-frac n d k)
  (define (cont-frac-inner i)
    (/ (n i)
       (if (= i k)
           (d i)
           (+ (d i)
              (cont-frac-inner (inc i))))))
  (cont-frac-inner 1))

(define (cont-frac* n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

(define (approximate-golden-ratio cont-frac k)
  (define approx (cont-frac (lambda (_) 1.0)
                            (lambda (_) 1.0)
                            k))

  (define error (abs (- (/ 1 golden-ratio) approx)))

  (display (format "k = ~a, approximation: ~a, error: ~a" k approx error))

  (if (< error 0.00005)
      (displayln " *** ok")
      (begin (newline) (approximate-golden-ratio cont-frac (inc k)))))

(module+ test)

(module+ main
  (displayln "cont-frac")
  (approximate-golden-ratio cont-frac 1)
  (displayln "cont-frac*")
  (approximate-golden-ratio cont-frac* 1))

#|
cont-frac
k = 1, approximation: 1.0, error: 0.3819660112501052
k = 2, approximation: 0.5, error: 0.11803398874989479
k = 3, approximation: 0.6666666666666666, error: 0.04863267791677184
k = 4, approximation: 0.6000000000000001, error: 0.018033988749894703
k = 5, approximation: 0.625, error: 0.0069660112501052085
k = 6, approximation: 0.6153846153846154, error: 0.0026493733652793727
k = 7, approximation: 0.6190476190476191, error: 0.0010136302977242773
k = 8, approximation: 0.6176470588235294, error: 0.0003869299263653536
k = 9, approximation: 0.6181818181818182, error: 0.00014782943192337417
k = 10, approximation: 0.6179775280898876, error: 5.646066000719596e-5
k = 11, approximation: 0.6180555555555556, error: 2.1566805660788724e-5 *** ok
cont-frac*
k = 1, approximation: 1.0, error: 0.3819660112501052
k = 2, approximation: 0.5, error: 0.11803398874989479
k = 3, approximation: 0.6666666666666666, error: 0.04863267791677184
k = 4, approximation: 0.6000000000000001, error: 0.018033988749894703
k = 5, approximation: 0.625, error: 0.0069660112501052085
k = 6, approximation: 0.6153846153846154, error: 0.0026493733652793727
k = 7, approximation: 0.6190476190476191, error: 0.0010136302977242773
k = 8, approximation: 0.6176470588235294, error: 0.0003869299263653536
k = 9, approximation: 0.6181818181818182, error: 0.00014782943192337417
k = 10, approximation: 0.6179775280898876, error: 5.646066000719596e-5
k = 11, approximation: 0.6180555555555556, error: 2.1566805660788724e-5 *** ok
|#