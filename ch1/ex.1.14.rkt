#lang racket/base

(require rackunit)

(define (function-call->string func-name args)
  (define (args->string result rest)
    (if (null? rest)
        result
        (args->string (format "~a ~v" result (car rest)) (cdr rest))))
  (format "(~a~a)" func-name (args->string "" args)))

(define (make-traced-function function-name function)
  ; (define (func self return ...) ...)
  (define (traced-function . args)
    (define (make-return args)
      (lambda (value)
        (displayln (format "  \"~a\" -> \"~v\""
                           (function-call->string function-name args)
                           value))
        value))

    (define (make-self args)
      (lambda next-args
        (displayln (format "  \"~a\" -> \"~a\""
                           (function-call->string function-name args)
                           (function-call->string function-name next-args)))
        (apply function
               (make-self next-args)
               (make-return next-args)
               next-args)))

    (apply function
           (make-self args)
           (make-return args)
           args))
  traced-function)

(define (first-denomination kinds-of-coins)
  (cond [(= kinds-of-coins 1) 1]
        [(= kinds-of-coins 2) 5]
        [(= kinds-of-coins 3) 10]
        [(= kinds-of-coins 4) 25]
        [(= kinds-of-coins 5) 50]))

(define (count-change amount)
  (define cc
    (make-traced-function
     "cc"
     (lambda (cc return amount kinds-of-coins)
       (cond [(= amount 0) (return 1)]
             [(or (< amount 0) (= kinds-of-coins 0)) (return 0)]
             [else (+ (cc amount
                            (- kinds-of-coins 1))
                      (cc (- amount (first-denomination kinds-of-coins))
                            kinds-of-coins))]))))

  (define result 0)

  (with-output-to-file "ex.1.14.dot"
    (lambda ()
      (displayln "digraph {")
      (set! result (cc amount 5))
      (displayln "}"))
    #:exists 'replace)

  result)

(check-= (count-change 11) 4 0)