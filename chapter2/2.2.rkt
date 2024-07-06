#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length* items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (cons (car xs) ys))))
  (iter items nil))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))

(define (no-more? coin-values) (null? coin-values))

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values))]))

(cc 100 us-coins)


(define (same-parity . items)
  (let ([parity (remainder (car items) 2)])
    (define (helper xs)
      (cond [(null? xs) '()]
            [(= (remainder (car xs) 2) parity)
             (cons (car xs) (helper (cdr xs)))]
            [else (helper (cdr xs))]))
    (helper items)))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

; (define (map proc items)
;   (if (null? items)
;       nil
;       (cons (proc (car items))
;             (map proc (cdr items)))))

(define (scale-list* items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list* items)
  (define (square x) (* x x))
  (map square items))

(define (square-list** items)
  (define (square x) (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; (square-list** '(1 2 3 4))

(define (for-each f items)
  (if (null? (cdr items))
      (f (car items))
      (begin (f (car items)) (for-each f (cdr items)))))

(define (count-leaves x)
  (cond [(null? x) 0]
        [(pair? x) (+ (count-leaves (car x)) (count-leaves (cdr x)))]
        [else 1]))

; (define a '(1 3 (5 7) 9))
; (define b '((7)))
; (define c '(1 (2 (3 (4 (5 (6 7)))))))

(define (deep-reverse items)
  (define (iter rest answer)
    (if (null? rest)
        answer
        (let ([cur (if (pair? (car rest))
                       (deep-reverse (car rest))
                       (car rest))])
          (iter (cdr rest) (cons cur answer)))))
  (iter items nil))

(define (fringe x)
  (cond [(null? x) '()]
        [(pair? x) (append (fringe (car x)) (fringe (cdr x)))]
        [else (list x)]))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (list-ref mobile 0))
(define (right-branch mobile) (list-ref mobile 1))
(define (branch-length branch) (list-ref branch 0))
(define (branch-structure branch) (list-ref branch 1))
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define mobile-x
  (make-mobile (make-branch 1 (make-mobile (make-branch 1 1)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 3)
                                           (make-branch 1 4)))))

(define mobile-balanced
  (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 4)
                                           (make-branch 1 4)))))

(define (is-balance mobile)
  (if (pair? mobile)
      (let ([left-len (branch-length (left-branch mobile))]
            [right-len (branch-length (right-branch mobile))]
            [left-struct (branch-structure (left-branch mobile))]
            [right-struct (branch-structure (right-branch mobile))])
        (and (= (* left-len (total-weight left-struct))
                (* right-len (total-weight right-struct)))
             (is-balance left-struct)
             (is-balance right-struct)))
      #t))


(define (scale-tree tree factor)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))]))

(define (scale-tree* tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square x) (* x x))

(define (square-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(define (square-tree* tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree** tree) (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (append (list (car s)) subset))
                          rest)))))

(define (sum-odd-squares tree)
  (cond [(null? tree) 0]
        [(not (pair? tree))
         (if (odd? tree) (square tree) 0)]
        [else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree)))]))

(define (even-fibs n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (define (fib n) (fib-iter 1 0 n))

  (define (next k)
    (if (> k n)
        nil
        (let ([f (fib k)])
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

(define (sum-odd-squares* tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs* n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (define (fib n) (fib-iter 1 0 n))

  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (map* p sequence)
  (accumulate (lambda (x rest) (cons (p x) rest)) nil sequence))

(define (append* seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length** sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (count-leaves* t)
  (accumulate + 0 (map (lambda (root)
                         (cond [(null? root) 0]
                               [(not (pair? root)) 1]
                               [else (+ (count-leaves (car root))
                                        (count-leaves (cdr root)))])) t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c)) cols)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (reverse* sequence)
  (fold-right (lambda (x y) (append y '(x))) nil sequence))

(define (reverse** sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))