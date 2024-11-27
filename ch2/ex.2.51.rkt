#lang racket/base

(provide beside
         below)

(require "ex.2.46.rkt"
         "ex.2.47.rkt"
         "ex.2.49.rkt"
         "ex.2.50.rkt")

(define (beside painter1 painter2)
  (let ([split-point (make-vect 0.5 0)])
    (let ([paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0))]
          [paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))])
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; 使用 transform-painter 实现: 略

(define (below painter-bottom painter-top)
  (rotate90 (beside (rotate270 painter-bottom) (rotate270 painter-top))))

(module+ test)

(module+ main
  (require support/drawing
           racket/runtime-path)

  (define-runtime-path beside.jpg "ex.2.51.beside.jpg")
  (define-runtime-path below.jpg "ex.2.51.below.jpg")

  (with-drawing-to-file beside.jpg '(776 640)
    ((beside wave rogers) frame-whole-canvas))
  (with-drawing-to-file below.jpg '(776 640)
    ((below wave rogers) frame-whole-canvas)))
