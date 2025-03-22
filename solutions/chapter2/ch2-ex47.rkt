#lang racket/base

(provide make-frame
         origin-frame
         edge1-frame
         edge2-frame
         frame-coord-map)

(require (only-in"ch2-ex46.rkt"
                 make-vect
                 xcor-vect
                 ycor-vect
                 add-vect
                 scale-vect))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-frame* origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame* car)
(define edge1-frame* cadr)
(define edge2-frame* cddr)

(module+ test
  (require akari-sicp/lib/testing)

  (define origin (make-vect 0 0))
  (define unit-x (make-vect 1 0))
  (define unit-y (make-vect 0 1))
  (define origin2 (make-vect 2 3))
  (define edge1 (make-vect 4 2))
  (define edge2 (make-vect 1 5))

  ;; Tests for the first implementation (using list)
  (define list-impl-tests
    (describe "list implementation of frame"
      (it "creates a frame and accesses components"
        (let ([frame (make-frame origin unit-x unit-y)])
          (expect
           [(origin-frame frame) => origin]
           [(edge1-frame frame) => unit-x]
           [(edge2-frame frame) => unit-y])))

      (it "works with arbitrary vectors"
        (let ([frame (make-frame origin2 edge1 edge2)])
          (expect
           [(origin-frame frame) => origin2]
           [(edge1-frame frame) => edge1]
           [(edge2-frame frame) => edge2])))

      (it "uses frame-coord-map correctly"
        (let* ([frame (make-frame origin2 edge1 edge2)]
               [mapper (frame-coord-map frame)])
          (expect
           [(mapper (make-vect 0 0)) => origin2]
           [(mapper (make-vect 1 0)) => (add-vect origin2 edge1)]
           [(mapper (make-vect 0 1)) => (add-vect origin2 edge2)]
           [(mapper (make-vect 1 1)) => (add-vect origin2 (add-vect edge1 edge2))])))))

  ;; Tests for the second implementation (using cons)
  (define cons-impl-tests
    (describe "cons implementation of frame"
      (it "creates a frame and accesses components"
        (let ([frame (make-frame* origin unit-x unit-y)])
          (expect
           [(origin-frame* frame) => origin]
           [(edge1-frame* frame) => unit-x]
           [(edge2-frame* frame) => unit-y])))

      (it "works with arbitrary vectors"
        (let ([frame (make-frame* origin2 edge1 edge2)])
          (expect
           [(origin-frame* frame) => origin2]
           [(edge1-frame* frame) => edge1]
           [(edge2-frame* frame) => edge2])))))

  (define all-tests
    (describe "frame implementations (exercise 2.47)"
      list-impl-tests
      cons-impl-tests))

  (run-tests all-tests))