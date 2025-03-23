#lang racket/base

(provide mcons/c
         mlistof
         mlist/c
         mcddr
         mcadr
         mcaddr
         mfirst
         msecond
         mthird
         (all-from-out compatibility/mlist))

(require racket/contract
         compatibility/mlist)

(define (mcons/c car-ctc cdr-ctc)
  (make-contract
   #:name (build-compound-type-name 'mcons/c car-ctc cdr-ctc)
   #:first-order mpair?
   #:projection
   (λ (blame)
     (define car-proj ((contract-projection car-ctc) blame))
     (define cdr-proj ((contract-projection cdr-ctc) blame))
     (λ (val)
       (unless (mpair? val)
         (raise-blame-error blame val '(expected: "mpair" given: "~e") val))
       (define car-val (mcar val))
       (define cdr-val (mcdr val))
       (define checked-car (car-proj car-val))
       (define checked-cdr (cdr-proj cdr-val))
       (if (and (eq? car-val checked-car)
                (eq? cdr-val checked-cdr))
           val
           (let ([new-pair (mcons checked-car checked-cdr)])
             new-pair))))))

(define (mlistof elem-ctc)
  (make-contract
   #:name (build-compound-type-name 'mlistof elem-ctc)
   #:first-order
   (λ (v)
     (or (null? v)
         (and (mpair? v)
              (let loop ([p v])
                (cond
                  [(null? p) #t]
                  [(mpair? p) (loop (mcdr p))]
                  [else #f])))))
   #:projection
   (λ (blame)
     (define elem-proj ((contract-projection elem-ctc) blame))
     (λ (val)
       (unless (or (null? val) (mpair? val))
         (raise-blame-error blame val '(expected: "mlist" given: "~e") val))
       (let loop ([v val])
         (cond
           [(null? v) null]
           [else
            (define car-val (mcar v))
            (define cdr-val (mcdr v))
            (define checked-car (elem-proj car-val))
            (define checked-cdr (loop cdr-val))
            (if (and (eq? car-val checked-car)
                     (eq? cdr-val checked-cdr))
                v
                (mcons checked-car checked-cdr))]))))))

(define (mlist/c . elem-ctcs)
  (define elem-count (length elem-ctcs))
  (make-contract
   #:name (apply build-compound-type-name 'mlist/c elem-ctcs)
   #:first-order
   (λ (v)
     (and (or (null? v) (mpair? v))
          (let loop ([p v] [n 0])
            (cond
              [(null? p) (= n elem-count)]
              [(mpair? p) (and (< n elem-count)
                               (loop (mcdr p) (add1 n)))]
              [else #f]))))
   #:projection
   (λ (blame)
     (define elem-projs
       (for/list ([elem-ctc (in-list elem-ctcs)])
         ((contract-projection elem-ctc) blame)))
     (λ (val)
       (unless (or (null? val) (mpair? val))
         (raise-blame-error blame val '(expected: "mlist" given: "~e") val))
       (let loop ([v val] [projs elem-projs] [pos 0])
         (cond
           [(null? projs)
            (unless (null? v)
              (raise-blame-error
               blame val
               '(expected: "mlist of ~a elements" given: "more than ~a elements")
               elem-count elem-count))
            null]
           [(null? v)
            (raise-blame-error
             blame val
             '(expected: "mlist of ~a elements" given: "~a elements")
             elem-count pos)]
           [else
            (define car-val (mcar v))
            (define cdr-val (mcdr v))
            (define proj (car projs))
            (define checked-car (proj car-val))
            (define checked-cdr (loop cdr-val (cdr projs) (add1 pos)))
            (if (and (eq? car-val checked-car)
                     (eq? cdr-val checked-cdr))
                v
                (mcons checked-car checked-cdr))]))))))

(define (mcddr x) (mcdr (mcdr x)))
(define (mcadr x) (mcar (mcdr x)))
(define (mcaddr x) (mcar (mcdr (mcdr x))))

(define mfirst mcar)
(define msecond mcadr)
(define mthird mcaddr)