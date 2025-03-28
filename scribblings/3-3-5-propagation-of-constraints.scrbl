#lang scribble/manual

@(require racket/file
          scribble/core
          scribble/html-properties
          scriblib/figure
          (for-label racket))

@title{Propagation of Constraints}

@section{Exercise 3.33}

Using primitive multiplier, adder, and constant constraints, define a procedure
@racket[averager] that takes three connectors @racket[a], @racket[b], and
@racket[c] as inputs and establishes the constraint that the value of @racket[c]
is the average of the values of @racket[a] and @racket[b].

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex33.rkt"))

@section{Exercise 3.34}

Louis Reasoner wants to build a squarer, a constraint device with two terminals
such that the value of connector @racket[b] on the second terminal will always
be the square of the value @racket[a] on the first terminal. He proposes the
following simple device made from a multiplier:

@racketblock[
(define (squarer a b)
  (multiplier a a b))
]

There is a serious flaw in this idea. Explain.

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex34.rkt"))

@section{Exercise 3.35}

Ben Bitdiddle tells Louis that one way to avoid the trouble in Exercise 3.34 is
to define a squarer as a new primitive constraint. Fill in the missing portions
in Ben's outline for a procedure to implement such a constraint:

@racketblock[
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            ⟨alternative1⟩)
        ⟨alternative2⟩))
  (define (process-forget-value) ⟨body1⟩)
  (define (me request) ⟨body2⟩)
  ⟨rest of definition⟩
  me)
]

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex35.rkt"))

@section{Exercise 3.36}

Suppose we evaluate the following sequence of expressions in the global
environment:

@racketblock[
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)
]

At some time during evaluation of the @racket[set-value!], the following
expression from the connector's local procedure is evaluated:

@racketblock[
(for-each-except
 setter inform-about-value constraints)
]

Draw an environment diagram showing the environment in which the above
expression is evaluated.

Solution:

@figure["ch3-ex36" @elem{The Environment when calling @tt{for-each-except}}]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter3/ch3-ex36.svg"]}

@section{Exercise 3.37}

The @racket[celsius-fahrenheit-converter] procedure is cumbersome when compared
with a more expression-oriented style of definition, such as

@racketblock[
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
]

Here @racket[c+], @racket[c*], etc. are the "constraint" versions of the
arithmetic operations. For example, @racket[c+] takes two connectors as
arguments and returns a connector that is related to these by an adder
constraint:

@racketblock[
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
]

Define analogous procedures @racket[c-], @racket[c*], @racket[c/], and
@racket[cv] (constant value) that enable us to define compound constraints as in
the converter example above.

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex37.rkt"))
