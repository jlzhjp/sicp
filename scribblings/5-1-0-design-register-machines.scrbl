#lang scribble/manual

@(require scribble/core
          scriblib/figure
          scribble/html-properties
          racket/file
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

@subsection{Exercise 5.2}

Use the register-machine language to describe the iterative factorial machine of Exercise 5.1

Solution:

@(verbatim (file->string "solutions/chapter5/ch5-ex02.txt"))

@subsection{Exercise 5.3}
Design a machine to compute square roots using Newton’s method, as described in 1.1.7:

@racketblock[
 (define (sqrt x)
   (define (good-enough? guess)
     (< (abs (- (square guess) x)) 0.001))
   (define (improve guess)
     (average guess (/ x guess)))
   (define (sqrt-iter guess)
     (if (good-enough? guess)
         guess
         (sqrt-iter (improve guess))))
   (sqrt-iter 1.0))]

Begin by assuming that @tt{good-enough?} and @tt{improve} operations are available as primitives. Then
show how to expand these in terms of arithmetic operations. Describe each version of the @tt{sqrt}
machine design by drawing a data-path diagram and writing a controller definition in the
register-machine language.

Solution:
@figure["ch5-ex03" (elem "Simple and Expanded Data Path of sqrt")]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter5/ch5-ex03.svg"]}

@(verbatim (file->string "solutions/chapter5/ch5-ex03.txt"))