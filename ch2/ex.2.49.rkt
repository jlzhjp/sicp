#lang racket/base

(provide outline
         x
         diamond
         wave)

(require support/drawing
         "ex.2.46.rkt"
         "ex.2.47.rkt"
         "ex.2.48.rkt")

(define outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define x
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
         (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

; https://stackoverflow.com/questions/13592352/
(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))


(module+ test)

(module+ main
  (require racket/runtime-path)

  (define-runtime-path outline.jpg "ex.2.49.outline.jpg")
  (define-runtime-path x.jpg "ex.2.49.x.jpg")
  (define-runtime-path diamond.jpg "ex.2.49.diamond.jpg")
  (define-runtime-path wave.jpg "ex.2.49.wave.jpg")

  (with-drawing-to-file outline.jpg '(128 128) 3
    (outline frame-whole-canvas))
  (with-drawing-to-file x.jpg '(128 128) 3
    (x frame-whole-canvas))
  (with-drawing-to-file diamond.jpg '(128 128) 3
    (diamond frame-whole-canvas))
  (with-drawing-to-file wave.jpg '(128 128) 3
    (wave frame-whole-canvas)))