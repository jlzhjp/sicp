#lang scribble/manual

@(require scribble/core
          scriblib/figure
          scribble/html-properties
          (for-label racket))

@title{Designing Register Machines}

@section{A Language for Describing Register Machines}

@subsection{Exercise 5.1}

Design a register machine to compute factorials using the iterative algorithm specified by the
following procedure. Draw data-path and controller diagrams for this machine.

@racketblock[
 (define (factorial n)
   (define (iter product counter)
     (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
   (iter 1 1))]

Solution:

@figure["ch5-ex01" (elem "Data Path and Controller of factorial")]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter5/ch5-ex01.svg"]}
