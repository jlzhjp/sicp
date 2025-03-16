#lang racket/base

(provide transform-painter
         flip-vert
         flip-horiz
         rotate90
         rotate180
         rotate270)

(require "ex.2.46.rkt"
         "ex.2.47.rkt")

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [new-origin (m origin)])
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(module+ test)

(module+ main
  (require racket/runtime-path
           support/drawing)

  (define-runtime-path flip-vert.jpg "ex.2.50.flip-vert.jpg")
  (define-runtime-path flip-horiz.jpg "ex.2.50.flip-horiz.jpg")
  (define-runtime-path squash-inwards.jpg "ex.2.50.squash-inwards.jpg")
  (define-runtime-path shrink-to-upper-right.jpg "ex.2.50.shrink-to-upper-right.jpg")
  (define-runtime-path rotate90.jpg "ex.2.50.rotate90.jpg")
  (define-runtime-path rotate180.jpg "ex.2.50.rotate180.jpg")
  (define-runtime-path rotate270.jpg "ex.2.50.rotate270.jpg")

  (with-drawing-to-file flip-vert.jpg '(640 776)
    (lambda ()
      ((flip-vert rogers) frame-whole-canvas)))

  (with-drawing-to-file flip-horiz.jpg '(640 776)
    (lambda ()
      ((flip-horiz rogers) frame-whole-canvas)))

  (with-drawing-to-file squash-inwards.jpg '(640 776)
    (lambda ()
      ((squash-inwards rogers) frame-whole-canvas)))

  (with-drawing-to-file shrink-to-upper-right.jpg '(640 776)
    (lambda ()
      ((shrink-to-upper-right rogers) frame-whole-canvas)))

  (with-drawing-to-file rotate90.jpg '(776 640)
    (lambda ()
      ((rotate90 rogers) frame-whole-canvas)))

  (with-drawing-to-file rotate180.jpg '(640 776)
    (lambda ()
      ((rotate180 rogers) frame-whole-canvas)))

  (with-drawing-to-file rotate270.jpg '(776 640)
    (lambda ()
      ((rotate270 rogers) frame-whole-canvas))))
