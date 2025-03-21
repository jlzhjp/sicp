#lang racket/base

(require "ch3-ex12.rkt"
         compatibility/mlist)

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z (make-cycle (mlist 'a 'b 'c)))
; (last-pair z) 不返回
