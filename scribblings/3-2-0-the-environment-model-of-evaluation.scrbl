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

In Section 1.2.1 we used the substitution model to analyze two procedures for
computing factorials, a recursive version:

@racketblock[
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))
]

and an iterative version:

@racketblock[
(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
  product
  (fact-iter (* counter product)
             (+ counter 1)
             max-count)))
]

Show the environment structures created by evaluating @racket[(factorial 6)]
using each version of @tt{factorial} procedure.


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
In the @tt{make-withdraw} procedure, the local
variable balance is created as a parameter of @tt{make-withdraw}.
We could also create the local state variable explicitly, using @racket[let],
as follows:

@racketblock[
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
]

Recall from Section 1.3.2 that @racket[let] is simply syntactic sugar for a procedure call:
@racketblock[
  (let ((var> <exp>)) <body>)
]

is equivalent to:
@racketblock[
  ((lambda (var) <body>) <exp>)
]

Use the environment model to analyze this alternate version
of @tt{make-withdraw}, drawing figures like the ones above
to illustrate the interactions

@racketblock[
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
]

Show that the two versions of make-withdraw create objects with the
same behavior. How do the environment structures differ for the two versions?

Solution:

@figure["ch3-ex10" @elem{Environment Model of the let Version @tt{make-withdraw}}]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter3/ch3-ex10.svg"]}

The let version of @tt{make-withdraw} cloned a frame in the environment chain.
When accessing the balance variable, the program found it in the frame created by @tt{let}
instead of the frame created by @tt{make-withdraw}.
And there is no behavioral difference between the two versions of @tt{make-withdraw}.


@section{Internal Definitions}

@subsection{Exercise 3.11}

In Section 3.2.3 we saw how the environment model described the behavior of procedures with local
state. Now we have seen how internal definitions work. A
typical message-passing procedure contains both of these
aspects. Consider the bank account procedure of Section 3.1.1:

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
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))

  dispatch)
]

Show the environment structure generated by the sequence of interactions

@racketblock[
(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)
]

Where is the local state for acc kept? Suppose we define another account

@racketblock[
(define acc2 (make-account 100))
]

How are the local states for the two accounts kept distinct? Which parts of the
environment structure are shared between @tt{acc} and @tt{acc2}?

Solution:

@figure["ch3-ex11" @elem{Environment Model of @tt{make-account}}]{
 @image[#:style (style #f (list (attributes '((style . "width: 100%; height: auto")))))
        "solutions/chapter3/ch3-ex11.svg"]}
