#lang racket/base

(require compatibility/mlist
         racket/contract
         racket/match
         ;; queue
        akari-sicp/solutions/chapter3/ch3-ex22)

;;; Time Segment

(define-struct/contract segment
  ([time integer?]
   [queue queue?])
  #:mutable)

;;; Agenda: a list of time segments

(provide after-delay)

(define-struct/contract agenda
  ([current-time integer?]
   [segments (mlistof segment?)])
  #:mutable)

(define/contract (first-segment agenda)
  (-> agenda? segment?)
  (mcar (agenda-segments agenda)))

(define/contract (rest-segments agenda)
  (-> agenda? (mlistof segment?))
  (mcdr (agenda-segments agenda)))

(define/contract (empty-agenda? agenda)
  (-> agenda? boolean?)
  (null? (agenda-segments agenda)))

;; check if the time is before the time of the first segment
(define/contract (belongs-before? time segments)
  (-> integer? (mlistof segment?) boolean?)
  ;; if the list is empty, return #t
  (or (null? segments)
      ;; if the time is less than the time of the first segment
      (< time (segment-time (mcar segments)))))

;; make a new time segment with time and a queue with one action
(define/contract (make-new-time-segment time action)
  (-> integer? procedure? segment?)
  (let ([q (make-queue)])
    (insert-queue! q action)
    (make-segment time q)))

;; add a new time and action to the list of segments
;; [note]: this function handle this situation when
;; `time` is greater than or equal to the time of the first segment
;; because this function doesn't return a new list
(define/contract (add-to-segments! time action segments)
  (-> integer? procedure? (mlistof segment?) void?)

  ;; if the time is the same as the time of the first segment
  (if (= (segment-time (mcar segments)) time)
      ;; then insert the action into the queue of the first segment
      (void (insert-queue! (segment-queue (mcar segments)) action))
      ;; if the time is just before the time of the first segment
      (let ([rest (mcdr segments)])
        ;; insert a new element to the list, with the new time and action
        (if (belongs-before? time rest)
            (set-mcdr!
             segments
             (mcons (make-new-time-segment time action)
                    (mcdr segments)))
            ;; otherwise, keep looking
            (add-to-segments! time action rest)))))

;; add a new time and action to the correct place in the agenda
(define/contract (add-to-agenda! time action agenda)
  (-> integer? procedure? agenda? void?)
  (define segments (agenda-segments agenda))
  (if (belongs-before? time segments)
      (set-agenda-segments!
       agenda
       (mcons (make-new-time-segment time action)
              segments))
      (add-to-segments! time action segments)))

(define the-agenda (make-parameter (make-agenda 0 '())))

(define/contract (after-delay delay action)
  (-> integer? procedure? void?)
  (add-to-agenda! (+ delay (agenda-current-time (the-agenda)))
                  action
                  (the-agenda)))

;;; Wires and Signals

(provide signal?
         wire?
         make-wire
         get-signal
         set-signal!
         add-action!)

(define signal? (one-of/c 0 1))

(define (wire? x) (eq? (car x) 'wire))

(define/contract (make-wire) (-> wire?)
  (define signal-value 0)
  (define action-procedures '())

  (define (call-each procedures)
    (unless (null? procedures)
      ((car procedures))
      (call-each (cdr procedures))))

  (define (set-my-signal! new-value)
    (unless (= signal-value new-value)
      (set! signal-value new-value)
      (call-each action-procedures)))

  (define (accept-action-procedure! proc)
    (set! action-procedures (cons proc action-procedures))
    (proc))

  (define (dispatch m)
    (match m
      ['get-signal signal-value]
      ['set-signal! set-my-signal!]
      ['add-action! accept-action-procedure!]
      [_ (error 'dispatch "Unknown operation")]))

  (list 'wire dispatch))

(define/contract (get-signal wire)
  (-> wire? signal?)
  ((cadr wire) 'get-signal))

(define/contract (set-signal! wire new-value)
  (-> wire? signal? void?)
  (((cadr wire) 'set-signal!) new-value))

(define/contract (add-action! wire action)
  (-> wire? procedure? void?)
  (((cadr wire) 'add-action!) action))

;;; Propagation

(provide propagate)

(define (propagate)
  (define agenda (the-agenda))
  (unless (empty-agenda? agenda)
    (let ([first-item (first-agenda-item agenda)])
      (first-item)
      (remove-first-agenda-item! agenda)
      ;; continue to propagate, until the agenda is empty
      (propagate))))


(define/contract (remove-first-agenda-item! agenda)
  (-> agenda? void?)

  ;; try to remove the first item from an empty agenda is an error
  ;; caller should check if the agenda is empty before calling this function
  (when (empty-agenda? agenda)
    (error 'remove-first-agenda-item! "Agenda is empty"))

  (define segment (first-segment agenda))
  (define q (segment-queue segment))

  ;; every segment must have at least one action by design
  ;; if it doesn't, something is wrong
  (when (empty-queue? q)
    (error 'remove-first-agenda-item! "Queue is empty"))

  (delete-queue! q)

  ;; if this time segment has no action after removal
  ;; then remove it from the agenda
  (when (empty-queue? q)
    (set-agenda-segments! agenda (rest-segments agenda))))


(define/contract (first-agenda-item agenda)
  (-> agenda? procedure?)
  (if (empty-agenda? agenda)
      (error 'first-agenda-item! "Agenda is empty")
      (let ([first-seg (first-segment agenda)])
        (set-agenda-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;; Logical Operations

(define/match (logical-not s)
  [(0) 1]
  [(1) 0]
  [(_) (error 'logical-not "Invalid signal value")])

(define/match (logical-and a b)
  [(0 0) 0]
  [(0 1) 0]
  [(1 0) 0]
  [(1 1) 1]
  [(_ _) (error 'logical-and "Invalid signal value")])

;;; Digital Circuits Components

(provide inverter
         probe
         and-gate)

(define inverter-delay 2)
(define and-gate-delay 3)

(define/contract (inverter input output)
  (-> wire? wire? void?)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(module+ test
  (require "testing.rkt")

  (define inverter-tests
    (let ([input (make-wire)]
          [output (make-wire)])
      (describe "test inverter"
        #:before (lambda () (inverter input output))

        (it "!0 = 1"
          (set-signal! input 0)
          (propagate)
          (expect [(get-signal output) => 1]))

        (it "!1 = 0"
          (set-signal! input 1)
          (propagate)
          (expect [(get-signal output) => 0]))))))

(define/contract (probe name wire)
  (-> symbol? wire? void?)
  (define (probe-action-procedure)
    (printf "\n~a Time = ~a, New Value = ~a"
            name
            (agenda-current-time (the-agenda))
            (get-signal wire)))
  (add-action! wire probe-action-procedure))

(define/contract (and-gate a1 a2 output)
  (-> wire? wire? wire? void?)
  (define (and-action-procedure)
    (define new-value (logical-and (get-signal a1) (get-signal a2)))
    (after-delay and-gate-delay
                 (lambda ()
                   (set-signal! output new-value))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(module+ test
  (define and-gate-tests
    (let ([a1 (make-wire)]
          [a2 (make-wire)]
          [output (make-wire)])
      (describe "test add gate"
        #:before (lambda () (and-gate a1 a2 output))

        (it "0 and 0 = 0"
          (set-signal! a1 0)
          (set-signal! a2 0)
          (propagate)
          (expect [(get-signal output) => 0]))

        (it "0 and 1 = 0"
          (set-signal! a1 0)
          (set-signal! a2 1)
          (propagate)
          (expect [(get-signal output) => 0]))

        (it "1 and 0 = 0"
          (set-signal! a1 1)
          (set-signal! a2 0)
          (propagate)
          (expect [(get-signal output) => 0]))

        (it "1 and 1 = 1"
          (set-signal! a1 1)
          (set-signal! a2 1)
          (propagate)
          (expect [(get-signal output) => 1]))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests
   (describe "test digital circuits"
     inverter-tests
     and-gate-tests)))