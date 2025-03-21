#lang racket/base

(require (only-in akari-sicp/lib/common
                  flatmap
                  enumerate-interval))


(define (make-position row col) (list row col))
(define row car)
(define col cadr)

; represents an empty set of positions
(define empty-board '())

; determines for a set of positions,
; whether the queen in the kth column is safe with respect to the others
; 去掉 k 参数，假设 positions 的第 1 个皇后就是要检查安全性的皇后
(define (safe? positions)
  (define queen-to-check (car positions))
  (define rest-queens (cdr positions))
  ; 由于是按列添加皇后的，因此不必检查是否有同列的
  (not (ormap (lambda (queen)
                (or (= (row queen) (row queen-to-check))
                    (= (abs (- (row queen) (row queen-to-check)))
                       (abs (- (col queen) (col queen-to-check))))))
              rest-queens)))

; adjoins a new row-column position to a set of positions,
(define (adjoin-position row k rest-of-queens)
  (cons (make-position row k) rest-of-queens))

(define (queens board-size)
  ; k: current column
  ; returns the sequence of all ways to place queens in the first k columns of the board
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? positions))
                ; rest-of-queens: a way to place k-1 queens in the first k-1 columns
                (flatmap (lambda (rest-of-queens)
                           ; new-row: proposed row in which to place the queen for the kth column
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(module+ test
  (require rackunit)

  (check-equal? (queens 0) '(()))
  (check-equal? (queens 1) '(((1 1))))
  (check-equal? (length (queens 8)) 92))