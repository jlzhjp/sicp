#lang racket/base

(provide square
         identity
         inc
         dec)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (identity x) x)