#lang racket/base

(require racket/contract
         akari-sicp/lib/digital-circuits
         (only-in "ch3-ex28.rkt" or-gate))

(define/contract (half-adder a b s c)
  (-> wire? wire? wire? wire? void?)
  (define d (make-wire))
  (define e (make-wire))
  (or-gate a b d)
  (and-gate a b c)
  (inverter c e)
  (and-gate d e s))

(define/contract (full-adder a b c-in sum c-out)
  (-> wire? wire? wire? wire? wire? void?)
  (define s (make-wire))
  (define c1 (make-wire))
  (define c2 (make-wire))
  (half-adder b c-in s c1)
  (half-adder a s sum c2)
  (or-gate c1 c2 c-out))

(define/contract (ripple-carry-adder as bs c-in ss c-out)
  (-> (listof wire?) (listof wire?) wire? (listof wire?) wire? void?)

  (define/contract (connect as bs c-in ss)
    (-> (listof wire?) (listof wire?) wire? (listof wire?) void?)
    (if (null? as)
        (add-action! c-in (lambda () (set-signal! c-out (get-signal c-in))))
        (let ([c-out-n (make-wire)])
          (full-adder (car as) (car bs) c-in (car ss) c-out-n)
          (connect (cdr as) (cdr bs) c-out-n (cdr ss)))))

  (connect as bs c-in ss))

(module+ test
  (require akari-sicp/lib/testing)

  ;; Half-adder tests
  (run-tests
   (let ([a (make-wire)]
         [b (make-wire)]
         [s (make-wire)]
         [c (make-wire)])
     (describe "test half-adder"
       #:before (lambda () (half-adder a b s c))

       (it "0 + 0 should output sum=0, carry=0"
         (set-signal! a 0)
         (set-signal! b 0)
         (propagate)
         (expect [(get-signal s) => 0]
                 [(get-signal c) => 0]))

       (it "1 + 0 should output sum=1, carry=0"
         (set-signal! a 1)
         (set-signal! b 0)
         (propagate)
         (expect [(get-signal s) => 1]
                 [(get-signal c) => 0]))

       (it "0 + 1 should output sum=1, carry=0"
         (set-signal! a 0)
         (set-signal! b 1)
         (propagate)
         (expect [(get-signal s) => 1]
                 [(get-signal c) => 0]))

       (it "1 + 1 should output sum=0, carry=1"
         (set-signal! a 1)
         (set-signal! b 1)
         (propagate)
         (expect [(get-signal s) => 0]
                 [(get-signal c) => 1])))))

  ;; Full-adder tests
  (run-tests
   (let ([a (make-wire)]
         [b (make-wire)]
         [c-in (make-wire)]
         [sum (make-wire)]
         [c-out (make-wire)])
     (describe "test full-adder"
       #:before (lambda () (full-adder a b c-in sum c-out))

       (it "0 + 0 + 0 should output sum=0, carry=0"
         (set-signal! a 0)
         (set-signal! b 0)
         (set-signal! c-in 0)
         (propagate)
         (expect [(get-signal sum) => 0]
                 [(get-signal c-out) => 0]))

       (it "1 + 0 + 0 should output sum=1, carry=0"
         (set-signal! a 1)
         (set-signal! b 0)
         (set-signal! c-in 0)
         (propagate)
         (expect [(get-signal sum) => 1]
                 [(get-signal c-out) => 0]))

       (it "0 + 1 + 0 should output sum=1, carry=0"
         (set-signal! a 0)
         (set-signal! b 1)
         (set-signal! c-in 0)
         (propagate)
         (expect [(get-signal sum) => 1]
                 [(get-signal c-out) => 0]))

       (it "0 + 0 + 1 should output sum=1, carry=0"
         (set-signal! a 0)
         (set-signal! b 0)
         (set-signal! c-in 1)
         (propagate)
         (expect [(get-signal sum) => 1]
                 [(get-signal c-out) => 0]))

       (it "1 + 1 + 0 should output sum=0, carry=1"
         (set-signal! a 1)
         (set-signal! b 1)
         (set-signal! c-in 0)
         (propagate)
         (expect [(get-signal sum) => 0]
                 [(get-signal c-out) => 1]))

       (it "1 + 0 + 1 should output sum=0, carry=1"
         (set-signal! a 1)
         (set-signal! b 0)
         (set-signal! c-in 1)
         (propagate)
         (expect [(get-signal sum) => 0]
                 [(get-signal c-out) => 1]))

       (it "0 + 1 + 1 should output sum=0, carry=1"
         (set-signal! a 0)
         (set-signal! b 1)
         (set-signal! c-in 1)
         (propagate)
         (expect [(get-signal sum) => 0]
                 [(get-signal c-out) => 1]))

       (it "1 + 1 + 1 should output sum=1, carry=1"
         (set-signal! a 1)
         (set-signal! b 1)
         (set-signal! c-in 1)
         (propagate)
         (expect [(get-signal sum) => 1]
                 [(get-signal c-out) => 1])))))

  ;; Ripple-carry-adder tests
  (run-tests
   (let ([a-wires (list (make-wire) (make-wire) (make-wire) (make-wire))]
         [b-wires (list (make-wire) (make-wire) (make-wire) (make-wire))]
         [sum-wires (list (make-wire) (make-wire) (make-wire) (make-wire))]
         [c-in (make-wire)]
         [c-out (make-wire)])
     (describe "test ripple-carry-adder (4-bit)"
       #:before (lambda () (ripple-carry-adder a-wires b-wires c-in sum-wires c-out))

       (it "0000 + 0000 + 0 should output sum=0000, carry=0"
         (for-each (lambda (wire) (set-signal! wire 0)) a-wires)
         (for-each (lambda (wire) (set-signal! wire 0)) b-wires)
         (set-signal! c-in 0)
         (propagate)
         (expect [(map get-signal sum-wires) => '(0 0 0 0)]
                 [(get-signal c-out) => 0]))

       (it "0101 + 0011 + 0 should output sum=1000, carry=0"
         (set-signal! (list-ref a-wires 0) 1)
         (set-signal! (list-ref a-wires 1) 0)
         (set-signal! (list-ref a-wires 2) 1)
         (set-signal! (list-ref a-wires 3) 0)

         (set-signal! (list-ref b-wires 0) 1)
         (set-signal! (list-ref b-wires 1) 1)
         (set-signal! (list-ref b-wires 2) 0)
         (set-signal! (list-ref b-wires 3) 0)

         (set-signal! c-in 0)
         (propagate)
         (expect [(map get-signal sum-wires) => '(0 0 0 1)]
                 [(get-signal c-out) => 0]))

       (it "1111 + 0001 + 0 should output sum=0000, carry=1"
         (for-each (lambda (wire) (set-signal! wire 1)) a-wires)
         (set-signal! (list-ref b-wires 0) 1)
         (set-signal! (list-ref b-wires 1) 0)
         (set-signal! (list-ref b-wires 2) 0)
         (set-signal! (list-ref b-wires 3) 0)
         (set-signal! c-in 0)
         (propagate)
         (expect [(map get-signal sum-wires) => '(0 0 0 0)]
                 [(get-signal c-out) => 1]))))))