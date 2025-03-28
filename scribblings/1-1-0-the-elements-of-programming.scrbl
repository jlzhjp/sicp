#lang scribble/manual

@(require racket/file
          scribble-math/dollar
          (for-label racket))

@title{The Elements of Programming}

@section{Expressions}

...

@section{Naming and Environment}

...

@section{Evaluating Combinations}

...

@section{The Substitution Model for Procedure Application}

The Substitution Model:

To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter
replaced by the corresponding argument.

Application Order versus Normal Order:

@itemlist[
 @item{Application Order: fully expand and then reduce}
 @item{Normal Order: evaluate the arguments and then apply}
 ]

@section{Conditional Expressions and Predicates}

@subsection{Exercise 1.1}

Below is a sequence of expressions. What is the result printed by the interpreter in response to each
expression? Assume that the sequence is to be evaluated in the order in which it is presented.

Solution:

@(codeblock (file->string "solutions/chapter1/ch1-ex01.rkt"))

@subsection{Exercise 1.2}

Translate the following expression into prefix form

@$${
 \frac{5 + 4 + (2 - (3 - (6 + \frac{4}{5})))}{3 (6 - 2) (2 - 7)}
}

Solution:

@(codeblock (file->string "solutions/chapter1/ch1-ex02.rkt"))

@subsection{Exercise 1.3}

Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two
larger numbers.

Solution:

@subsection{Exercise 1.4}

Observe that our model of evaluation allows for combinations whose operators are compound expressions.
Use this observation to describe the behavior of the following procedure:

@racketblock[
 (define (a-plus-abs-b a b)
   ((if (> b 0) + -) a b))]

Solution:

@itemlist[
 @item{if @tt{b > 0}, then apply the parameters @racket[a] and @racket[b] to the
  function @racket[+], produce the result of @tt{a + b}}
 @item{if @tt{b < 0}, then apply the parameters @racket[a] and @racket[b] to the
  function @racket[-], produce the result of @tt{a - b}}]

@subsection{Exercise 1.5}

Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using
applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

@racketblock[
 (define (p) (p))
 (define (test x y)
   (if (= x 0) 0 y))]

Then he evaluates the expression

@racketblock[
 (test 0 (p))]

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior
will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that
the evaluation rule for the special form @racket[if] is the same whether the interpreter is using normal
or applicative order: The predicate expression is evaluated first, and the result determines whether to
evaluate the consequent or the alternative expression.)

Solution:

@itemlist[
 @item{Applicative-order evaluation: the interpreter evaluate the arguments
  first, which means it will evaluate @racket[(p)] first, producing an infinite loop}
 @item{Normal-order evaluation: the interpreter will evaluate the predicate first,
  which is @racket[0], and then return @racket[0] without evaluating @racket[(p)]}]

@subsection{Exercise 1.6}

Alyssa P. Hacker doesn't see why @racket[if] needs to be provided as a special form.
"Why can't I just define it as an ordinary procedure in terms of @racket[cond]?" she asks.
Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of @racket[if]:

@racketblock[
 (define (new-if predicate then-clause else-clause)
   (cond (predicate then-clause)
         (else else-clause)))]

Eva demonstrates the program for Alyssa:

@racketblock[
 (new-if (= 2 3) 0 5)
 5
 (new-if (= 1 1) 0 5)
 0]

Delighted, Alyssa uses @racket[new-if] to rewrite the square-root program:

@racketblock[
 (define (sqrt-iter guess x)
   (new-if (good-enough? guess x)
           guess
           (sqrt-iter (improve guess x) x)))]

What happens when Alyssa attempts to use this to compute square roots? Explain.

Answer:

Scheme uses applicative-order evaluation by default, so the @racket[then-clause] and @racket[else-clause]
of @racket[new-if] are evaluated when passing them to @racket[new-if] before the predicate is evaluated,
which leads to an infinite loop.

@subsection{Exercise 1.7}

The @racket[good-enough?] test used in computing square roots will not be very effective for finding the
square roots of very small numbers. Also, in real computers, arithmetic operations are almost always
performed with limited precision. This makes our test inadequate for very large numbers. Explain these
statements, with examples showing how the test fails for small and large numbers. An alternative strategy
for implementing @racket[good-enough?] is to watch how @racket[guess] changes from one iteration to the
next and to stop when the change is a very small fraction of the guess. Design a square-root procedure
that uses this kind of end test. Does this work better for small and large numbers?

Solution:

@(codeblock (file->string "solutions/chapter1/ch1-ex07.rkt"))

@subsection{Exercise 1.8}

Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x,
then a better approximation is given by the value

@$${
 \frac{x/y^2 + 2y}{3}
}

Use this formula to implement a cube-root procedure analogous to the square-root procedure.

Solution:

@(codeblock (file->string "solutions/chapter1/ch1-ex08.rkt"))
