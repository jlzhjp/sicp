#lang racket/base

(require "ex.2.67.rkt"
         "ex.2.68.rkt"
         "ex.2.69.rkt")

(define song-word-frequency
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define song-huffman-tree (generate-huffman-tree song-word-frequency))

(define song
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(module+ test
  (require support/testing)

  (define encoded-song (encode song song-huffman-tree))

  (check-= (length encoded-song) 84 0)
  (check-= (* 3 (length song)) 108 0)


  (check-equal? encoded-song
                '(1 1 1 1 1 1 1 0 0 1
                    1 1 1 0 1 1 1 0 0 0
                    0 0 0 0 0 0 1 1 1 1 1
                    1 1 0 0 1 1 1 1 0 1 1
                    1 0 0 0 0 0 0 0 0 0 1
                    1 0 1 0 1 0 1 0 1 0 1
                    0 1 0 1 0 1 0 1 0 1 0
                    1 1 1 0 1 1 0 1 1))

  (check-equal? (decode encoded-song song-huffman-tree)
                song))