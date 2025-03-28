#lang racket/base

(provide inform-about-value
         inform-about-no-value
         has-value?
         get-value
         set-value!
         forget-value!
         connect
         make-connector
         adder
         multiplier
         probe
         constant
         celsius-fahrenheit-converter)

(require racket/match)

;; the interface a constraint must implement

;; notify a connector has a new value
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

;; notify a connector has lost its value
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; the interface a connector must implement

;; tells whether the connector has a value
(define (has-value? connector)
  (connector 'has-value?))

;; returns the connector's current value
;; indicates that the informant is requesting
;; the connector to set its value to the new value
(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

;; tells the connector that the retractor is
;; requesting it to forget its value
(define (forget-value! connector informant)
  ((connector 'forget) informant))

;; tells the connector to participate in the new constraint
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; connector constructor
(define (make-connector)
  (define value #f)
  (define informant #f)
  (define constraints '())

  (define (set-my-value newval setter)
    ;; if there is no value, then set it
    (cond [(not (has-value? me))
           (set! value newval)
           (set! informant setter)
           ;; notify all constraints the value has changed (except the setter)
           (for-each-except setter inform-about-value constraints)]
          [(not (= value newval))
           (error "contradiction" (list value newval))]
          [else (void)]))

  (define (forget-my-value retractor)
    ;; only drop the value if it was set by the retractor
    (when (eq? retractor informant)
      (set! informant #f)
      (for-each-except retractor inform-about-no-value constraints)))

  (define (connect new-constraint)
    (when (not (memq new-constraint constraints))
      (set! constraints (cons new-constraint constraints)))
    (when (has-value? me)
      (inform-about-value new-constraint)))

  (define me
    (match-lambda
      ['has-value? (if informant #t #f)]
      ['value value]
      ['set-value! set-my-value]
      ['forget forget-my-value]
      ['connect connect]
      [otherwise (error 'connector "unknown operation ~a" otherwise)]))
  me)

;; utility functions
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) (void)]
          [(eq? (car items) exception) (loop (cdr items))]
          [else
           (procedure (car items))
           (loop (cdr items))]))
  (loop list))

;; predefined constraints

(define (adder a1 a2 sum)
  ;; member functions
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me)]))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  ;; dispatcher
  (define me
    (match-lambda
      ['I-have-a-value (process-new-value)]
      ['I-lost-my-value (process-forget-value)]
      [otherwise (error 'adder "unknown request ~a" otherwise)]))

  ;; initialize the connector
  (connect a1 me)
  (connect a2 me)
  (connect sum me)

  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me)]
          [(and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me)]
          [(and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me)]))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define me
    (match-lambda
      ['I-have-a-value (process-new-value)]
      ['I-lost-my-value (process-forget-value)]
      [otherwise (error 'multiplier "unknown request ~a" otherwise)]))

  (connect m1 me)
  (connect m2 me)
  (connect product me)

  me)

(define (probe name connector)
  (define (print-probe value)
    (printf "Probe: ~a = ~a\n" name value))

  (define (process-new-value)
    (print-probe (get-value connector)))

  (define (process-forget-value) (print-probe "?"))

  (define me
    (match-lambda
      ['I-have-a-value (process-new-value)]
      ['I-lost-my-value (process-forget-value)]
      [otherwise (error 'probe "unknown request ~a" otherwise)]))

  (connect connector me)

  me)

(define (constant value connector)
  (define (me request)
    (error 'constant "unknown request ~a" request))

  (connect connector me)
  (set-value! connector value me)

  me)

;; composite constraints
(define (celsius-fahrenheit-converter c f)
  ;; 9C = 5(F - 32)
  (define _9 (make-connector))
  (define _5 (make-connector))
  (define _32 (make-connector))
  (define u (make-connector))
  (define v (make-connector))

  (constant 9 _9)
  (constant 5 _5)
  (constant 32 _32)

  ;; 9C = 5V
  (multiplier _9 c u)
  (multiplier _5 v u)

  ;; V + 32 = F
  (adder v _32 f))

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "Constraint System Tests"
     (describe "adder constraint"
       (it "propagates from a1 and a2 to sum"
         (define a1 (make-connector))
         (define a2 (make-connector))
         (define sum (make-connector))
         (adder a1 a2 sum)
         (probe "sum" sum)
         (set-value! a1 5 'user)
         (expect [(has-value? sum) => #f]
                 [(set-value! a2 7 'user) =$> '("Probe: sum = 12")]
                 [(get-value sum) => 12]
                 [(has-value? sum) => #t]))

       (it "propagates from a1 and sum to a2"
         (define a1 (make-connector))
         (define a2 (make-connector))
         (define sum (make-connector))
         (adder a1 a2 sum)
         (probe "a2" a2)
         (expect [(set-value! a1 8 'user) =$> '()]
                 [(set-value! sum 15 'user) =$> '("Probe: a2 = 7")]
                 [(get-value a2) => 7]
                 [(has-value? a2) => #t]))

       (it "propagates from a2 and sum to a1"
         (define a1 (make-connector))
         (define a2 (make-connector))
         (define sum (make-connector))
         (adder a1 a2 sum)
         (probe "a1" a1)
         (expect [(set-value! a2 9 'user) =$> '()]
                 [(set-value! sum 20 'user) =$> '("Probe: a1 = 11")]
                 [(get-value a1) => 11]
                 [(has-value? a1) => #t]))

       (it "raises error on contradiction"
         (define a1 (make-connector))
         (define a2 (make-connector))
         (define sum (make-connector))
         (adder a1 a2 sum)
         (set-value! a1 5 'user)
         (set-value! a2 7 'user)
         (expect [(set-value! sum 15 'user) =!> #rx"contradiction"])))

     (describe "multiplier constraint"
       (it "propagates from m1 and m2 to product"
         (define m1 (make-connector))
         (define m2 (make-connector))
         (define product (make-connector))
         (multiplier m1 m2 product)
         (probe "product" product)
         (set-value! m1 3 'user)
         (expect [(set-value! m2 4 'user) =$> '("Probe: product = 12") ]
                 [(get-value product) => 12]
                 [(has-value? product) => #t]))

       (it "propagates from m1 and product to m2"
         (define m1 (make-connector))
         (define m2 (make-connector))
         (define product (make-connector))
         (multiplier m1 m2 product)
         (probe "m2" m2)
         (set-value! m1 5 'user)
         (expect [(set-value! product 20 'user) =$> '("Probe: m2 = 4")]
                 [(get-value m2) => 4]
                 [(has-value? m2) => #t]))

       (it "propagates from m2 and product to m1"
         (define m1 (make-connector))
         (define m2 (make-connector))
         (define product (make-connector))
         (multiplier m1 m2 product)
         (probe "m1" m1)
         (set-value! m2 2 'user)
         (expect [(set-value! product 10 'user) =$> '("Probe: m1 = 5")]
                 [(get-value m1) => 5]
                 [(has-value? m1) => #t]))

       (it "handles zero values correctly"
         (define m1 (make-connector))
         (define m2 (make-connector))
         (define product (make-connector))
         (multiplier m1 m2 product)
         (probe "product" product)
         (expect [(set-value! m1 0 'user) =$> '("Probe: product = 0")]
                 [(set-value! m2 5 'user) =$> '()]
                 [(get-value product) => 0]))

       (it "raises error on contradiction"
         (define m1 (make-connector))
         (define m2 (make-connector))
         (define product (make-connector))
         (multiplier m1 m2 product)
         (set-value! m1 3 'user)
         (set-value! m2 4 'user)
         (expect [(set-value! product 15 'user) =!> #rx"contradiction"])))

     (describe "complex constraint network"
       (it "celsius-fahrenheit-converter converts C to F"
         (define c (make-connector))
         (define f (make-connector))
         (celsius-fahrenheit-converter c f)
         (probe "f" f)
         (expect [(set-value! c 25 'user) =$> '("Probe: f = 77")]
                 [(get-value f) => 77]))

       (it "celsius-fahrenheit-converter converts F to C"
         (define c (make-connector))
         (define f (make-connector))
         (celsius-fahrenheit-converter c f)
         (probe "c" c)
         (expect [(set-value! f 68 'user) =$> '("Probe: c = 20")]
                 [(get-value c) => 20]))))))