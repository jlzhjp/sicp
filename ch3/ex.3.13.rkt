#lang racket/base

(require "ex.3.12.rkt"
         compatibility/mlist)

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z (make-cycle (mlist 'a 'b 'c)))
; (last-pair z) 不返回
