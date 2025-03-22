#lang racket/base

(provide beside
         below
         below*)

(require akari-sicp/lib/picture
         (only-in "ch2-ex50.rkt" transform-painter rotate90 rotate270))

(define (beside painter1 painter2)
  (let ([painter-left
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 0.5 0.0)
                            (make-vect 0.0 1.0))]
        [painter-right
         (transform-painter painter2
                            (make-vect 0.5 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.5 1.0))])
    (lambda (frame)
      (painter-left frame)
      (painter-right frame))))


;; implement the below function using transform-painter
(define (below painter1 painter2)
  (let ([painter-bottom
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 0.5))]
        [painter-top
         (transform-painter painter2
                            (make-vect 0.0 0.5)
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0))])
    (lambda (frame)
      (painter-bottom frame)
      (painter-top frame))))

;; implement the below* function using beside and suitable rotations
(define (below* painter-bottom painter-top)
  (rotate90 (beside (rotate270 painter-bottom) (rotate270 painter-top))))
