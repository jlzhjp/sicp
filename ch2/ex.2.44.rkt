#lang racket/base

(require support
         "ex.2.45.rkt"
         "ex.2.47.rkt"
         (only-in "ex.2.49.rkt" wave)
         "ex.2.50.rkt"
         "ex.2.51.rkt")

; (define (right-split painter n)
;   (if (= n 0)
;       painter
;       (let ([smaller (right-split painter (- n 1))])
;         (beside painter (below smaller smaller)))))

; (define (up-split painter n)
;   (if (= n 0)
;       painter
;       (let ([smaller (up-split painter (- n 1))])
;         (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ([up (up-split painter (- n 1))]
             [right (right-split painter (- n 1))]
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [corner (corner-split painter (- n 1))])
        (beside (below painter top-left)
                (below bottom-right corner)))))

; (define (square-limit painter n)
;   (let* ([quarter (corner-split painter n)]
;          [half (beside (flip-horiz quarter) quarter)])
;     (below (flip-vert half) half)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))))

(module+ test)

(module+ main
  (require support/drawing
           racket/runtime-path)

  (define-runtime-path wave.jpg "ex.2.44.wave.jpg")
  (define-runtime-path rogers.jpg "ex.2.44.rogers.jpg")

  (with-drawing-to-file wave.jpg '(512 512)
    ((square-limit wave 4) frame-whole-canvas))

  (with-drawing-to-file rogers.jpg '(640 776)
    ((square-limit rogers 4) frame-whole-canvas)))