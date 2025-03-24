#lang scribble/manual

@(require scribble/core
          scribble/html-properties
          scriblib/figure
          (for-label racket))

@title{The Environment Model of Evaluation}

@section{The Rules for Evaluation}

Concepts:

@itemize[
  @item{@italic{Environment} : A sequence of @italic{Frames}, in which values can
        be stored.}
  @item{@italic{Frame}: A table (possibly empty) of bindings, which associate
        variable names with their corresponding values.}
  @item{@italic{Procedure}: A pair consisting of @bold{some code} and @bold{a
        pointer to an @italic{Environment}.}
        @italic{Procedure}s are created in one way only: by evaluation of a lambda
        expression.
        @itemize{
          @item{@italic{Code} is obtained from the text of the lambda expression.}
          @item{@italic{Environment} is the environment in which the lambda
                expression was evaluated to produce the procedure.}
        }}]

The Rules for Evaluation:

@itemize[
 @item{@bold{Rule 1}:
  A @italic{procedure} object is applied to a set of arguments by constructing a
  @italic{frame} binding the formal parameters of the call, and then
  evaluating the body of the @italic{procedure} in the context of the new
  @italic{environment} constructed. The new @italic{frame} has as its enclosing
  @italic{environment} the @italic{environment} part of the @italic{procedure}
  object being applied.}

 @item{@bold{Rule 2}:
  A lambda-expression is evaluated relative to a given @italic{environment} as
  follows: a new @italic{procedure} object is formed, combining the text (code)
  of the lambda-expression with a pointer to the @italic{environment} of
  evaluation.}]

Actions and Identity:

@itemize[
 @item{
  We say that an action, A, had an effect on an object, X, (or equivalently, that
  X was changed by A) if some property, P, which was true of X before A became
  false of X after A.}
 @item{
  We say that two objects, X and Y, are the same if any action which has
  an effect on X has the same effect on Y.}]


@section{Apply Simple Procedures}

@subsection{Exercise 3.9}

In section 1.2.1 we used the substitution model to analyze two procedures for
computing factorials, a recursive version:

@racketblock[
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
]

and an iterative version that uses an accumulator:

@racketblock[
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
]

Show the environment structures created by evaluating @racket[(factorial 6)]
using each version.


Solution:

Recursive Version:

@figure["ch3-ex09-rec" (elem "Environment Model of the Recursive Version")]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter3/ch3-ex09-rec.svg"]}

Iterative Version:

@figure["ch3-ex09-iter" (elem "Environment Model of the Iterative Version")]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter3/ch3-ex09-iter.svg"]}


@section{Frames as the Repository of Local State}

@subsection{Exercise 3.10}

In section 3.2.3, we saw how the environment model describes the behavior of
procedures with local state. Now we examine how let expressions are handled.

Consider the @racket[make-withdraw] procedure with let expressions:

@racketblock[
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
]

This differs from our earlier version which used lambda to bind the local state:

@racketblock[
(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))
]

Use the environment model to analyze the evaluation of:

@racketblock[
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
]

Show the resulting environment structure. Does this differ from the structure
created by the original @racket[make-withdraw] procedure? Explain.

The let expression creates a frame with the binding for @racket[balance], while
the lambda version creates two frames: one for the outer lambda parameter and
another for the returned procedure. Despite this difference, the behavior
remains the same, as both maintain state in an environment frame.

@section{Internal Definitions}

@subsection{Exercise 3.11}

Consider the following bank account procedure with an internal definition:

@racketblock[
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
]

Use the environment model to describe the implementation of
@racket[make-account], analyzing:

@racketblock[
(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)
]

Where is the local state for @racket[acc] kept? How are the local
procedures @racket[deposit] and @racket[withdraw] represented? Suppose we create
another account:

@racketblock[
(define acc2 (make-account 100))
]

How are the local state and procedures for @racket[acc2] different from those
for @racket[acc]?

The local state for each account is maintained in the environment frame created
when @racket[make-account] is called. The internal procedures @racket[withdraw],
@racket[deposit], and @racket[dispatch] all share this environment, allowing
them to access and modify the same @racket[balance] variable. Each new account
creates a distinct environment structure, ensuring separate state.