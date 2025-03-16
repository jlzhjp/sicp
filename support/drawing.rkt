#lang racket/base

;; Drawing utilities for SICP graphics
;; Provides functionality for creating and manipulating drawings

(provide with-drawing-to-file
         segments->painter
         bitmap->painter
         rogers)

(require racket/class
         racket/draw
         racket/match
         racket/runtime-path

         "../ch2/ex.2.46.rkt"
         "../ch2/ex.2.47.rkt"
         "../ch2/ex.2.48.rkt")

(define (make-transform-matrix . vs)
  (match vs [(list (vector xx xy) (vector yx yy) (vector x0 y0))
             (vector xx xy yx yy x0 y0)]))

(define identity-matrix
  (make-transform-matrix
   (vector 1 0)
   (vector 0 1)
   (vector 0 0)))

(struct drawing-context
  (rkt-bitmap
   rkt-dc
   canvas-width
   canvas-height))

(define (make-drawing-context canvas-width canvas-height)
  (define rkt-bitmap (make-bitmap canvas-width canvas-height))
  (define rkt-dc (new bitmap-dc% [bitmap rkt-bitmap]))
  (send rkt-dc set-background "white")
  (send rkt-dc clear)
  (send rkt-dc set-smoothing 'aligned)
  (drawing-context rkt-bitmap rkt-dc canvas-width canvas-height))

(define current-drawing-context (make-parameter '()))

(define (check-drawing-context)
  (when (null? (current-drawing-context))
    (error "no drawing context provided")))

(define (with-drawing-to-file filename size thunk)
  ;; Function for creating and saving drawings to a file
  ;; Parameters:
  ;; - filename: Path where to save the drawing
  ;; - size: A list with width and height
  ;; - thunk: Procedure containing drawing commands
  (define width (car size))
  (define height (cadr size))

  (parameterize ([current-drawing-context (make-drawing-context width height)])
    (thunk)
    (send (drawing-context-rkt-bitmap (current-drawing-context)) save-file filename 'jpeg))

  (void))

(define (draw-line x1 y1 x2 y2)
  ;; Draws a line between two points
  ;; Parameters:
  ;; - x1, y1: Starting point coordinates (0.0-1.0)
  ;; - x2, y2: Ending point coordinates (0.0-1.0)
  (check-drawing-context)
  (define context (current-drawing-context))

  (match-define (drawing-context _ rkt-dc w h) context)

  (send rkt-dc transform
        (make-transform-matrix
         (vector 1 0)
         (vector 0 -1)
         (vector 0 h)))
  (send rkt-dc set-pen "black" 1 'solid)
  (send rkt-dc draw-line (* x1 w) (* y1 h) (* x2 w) (* y2 h))
  (send rkt-dc set-initial-matrix identity-matrix))

(define (segments->painter segment-list)
  ;; Creates a painter that draws a set of line segments
  ;; Parameters:
  ;; - segment-list: List of segments to draw
  ;; Returns a procedure that draws the segments in a given frame
  (lambda (frame)
    (define coord-map (frame-coord-map frame))
    (for-each
     (lambda (segment)
       (define start-vect (coord-map (start-segment segment)))
       (define end-vect (coord-map (end-segment segment)))
       (draw-line
        (xcor-vect start-vect)
        (ycor-vect start-vect)
        (xcor-vect end-vect)
        (ycor-vect end-vect)))
     segment-list)))

(define (draw-bitmap bitmap ox oy e1x e1y e2x e2y)
  ;; Draws a bitmap with specified coordinates and transformation
  ;; Parameters:
  ;; - bitmap: The bitmap to draw
  ;; - ox, oy: Origin coordinates
  ;; - e1x, e1y, e2x, e2y: Edge vectors for transformation
  (check-drawing-context)
  (define context (current-drawing-context))

  (define width (send bitmap get-width))
  (define height (send bitmap get-height))

  (define flipped-bitmap (make-object bitmap% width height))
  (define flipped-dc (new bitmap-dc% [bitmap flipped-bitmap]))
  (send flipped-dc set-initial-matrix (vector 1 0 0 -1 0 height))
  (send flipped-dc draw-bitmap bitmap 0 0)

  (match-define (drawing-context _ rkt-dc cw ch) context)
  (send rkt-dc transform
        (make-transform-matrix
         (vector cw 0)
         (vector 0 ch)
         (vector 0 0)))

  (send rkt-dc transform
        (make-transform-matrix
         (vector 1 0)
         (vector 0 -1)
         (vector 0 1)))

  (send rkt-dc transform
        (make-transform-matrix
         (vector e1x e1y)
         (vector e2x e2y)
         (vector ox oy)))

  (send rkt-dc
        draw-bitmap-section-smooth
        flipped-bitmap
        0 0
        1 1
        0 0
        width
        height)
  (send rkt-dc set-initial-matrix identity-matrix))

(define (bitmap->painter filename)
  ;; Creates a painter from a bitmap file
  ;; Parameter:
  ;; - filename: Path to bitmap file
  ;; Returns a procedure that draws the bitmap in a given frame
  (define bitmap (read-bitmap filename))

  (lambda (frame)
    (define origin (origin-frame frame))
    (define edge1 (edge1-frame frame))
    (define edge2 (edge2-frame frame))
    (draw-bitmap bitmap
                 (xcor-vect origin)
                 (ycor-vect origin)
                 (xcor-vect edge1)
                 (ycor-vect edge1)
                 (xcor-vect edge2)
                 (ycor-vect edge2))))

;; Path to the William Barton Rogers image
(define-runtime-path rogers-path "William_Barton_Rogers.jpg")

;; Predefined painter using the Rogers image
(define rogers (bitmap->painter rogers-path))
