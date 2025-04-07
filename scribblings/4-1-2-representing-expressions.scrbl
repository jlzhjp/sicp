#lang scribble/manual

@(require racket/file
          (for-label racket))

@title{Representing Expressions}


@section{Exercise 4.3}

Rewrite eval so that the dispatch is done in data-directed style. Compare this
with the data-directed differentiation procedure of Exercise 2.73. (You may use
the car of a compound expression as the type of the expression, as is appropriate
for the syntax implemented in this section.)

@(codeblock (file->string "solutions/chapter4/ch4-ex03.rkt"))


@section{Exercise 4.4}

Install and and or as new special forms for the evaluator by defining appropriate
syntax procedures and evaluation procedures @tt{eval-and} and @tt{eval-or}.
Alternatively, show how to implement and and or as derived expressions.

@(codeblock (file->string "solutions/chapter4/ch4-ex04.rkt"))


@section{Exercise 4.5}

Scheme allows an additional syntax for @tt{cond}
clauses, @tt{(⟨test⟩ => ⟨recipient⟩)}. If @tt{⟨test⟩} evaluates to a
true value, then @tt{⟨recipient⟩} is evaluated. Its value must be a
procedure of one argument; this procedure is then invoked
on the value of the @tt{⟨test⟩}, and the result is returned as the
value of the @tt{cond} expression. For example

@codeblock{
 (cond ((assoc 'b '((a 1) (b 2))) => cadr)
 (else false))
}
returns 2. Modify the handling of @tt{cond} so that it supports
this extended syntax.

@(codeblock (file->string "solutions/chapter4/ch4-ex05.rkt"))


@section{Exercise 4.6}

Let expressions are derived expressions, because
@racketblock[
 (let ((<var₁> <exp₁>) ... (<varₙ> <expₙ>))
   <body>)]

is equivalent to
@racketblock[
 ((lambda (<var₁> ... <varₙ>)
    <body>)
  <exp₁>
  ...
  <expₙ>)]

Implement a syntactic transformation @tt{let->combination} that reduces @tt{let}
expressions to procedure calls as shown, and add the appropriate clause to
@tt{eval} to handle @tt{let} expressions.

@(codeblock (file->string "solutions/chapter4/ch4-ex06.rkt"))

@section{Exercise 4.7}
@tt{let*} is similar to @tt{let}, except that the bindings of the @tt{let*} variables are performed
sequentially from left to right, and each binding is made in an environment in which all of the
preceding bindings are visible. For example
@racketblock[
 (let* ((x 3)
        (y (+ x 2))
        (z (+ x y 5)))
   (* x z))]

returns @tt{39}. Explain how a @tt{let*} expression can be rewritten as a set of nested let
expressions, and write a procedure @tt{let*->nested-lets} that performs this transformation. If we
have already implemented @tt{let} (Exercise 4.6) and we want to extend the evaluator to handle
@tt{let*}, is it sufficient to add a clause to eval whose action is

@racketblock[
 (eval (let*->nested-lets exp) env)]

or must we explicitly expand @tt{let*} in terms of non-derived expressions?

Solution:

@(codeblock (file->string "solutions/chapter4/ch4-ex07.rkt"))

@section{Exercise 4.8}
“Named let” is a variant of @tt{let} that has the
form

@racketblock[
 (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)]

The @tt{⟨bindings⟩} and @tt{⟨body⟩} are just as in ordinary @tt{let}, except
that @tt{⟨var⟩} is bound within @tt{⟨body⟩} to a procedure whose
body is @tt{⟨body⟩} and whose parameters are the variables in
the @tt{⟨bindings⟩}. Thus, one can repeatedly execute the @tt{⟨body⟩}
by invoking the procedure named @tt{⟨var⟩}. For example, the
iterative Fibonacci procedure (Section 1.2.2) can be rewritten
using named let as follows:

@racketblock[
 (define (fib n)
   (let fib-iter ((a 1)
                  (b 0)
                  (count n))
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1)))))]

Modify @tt{let->combination} of Exercise 4.6 to also support
named @tt{let}.

Solution:

@(codeblock (file->string "solutions/chapter4/ch4-ex08.rkt"))

@section{Exercise 4.9}
Many languages support a variety of iteration
constructs, such as do, for, while, and until. In Scheme,
iterative processes can be expressed in terms of ordinary
procedure calls, so special iteration constructs provide no
essential gain in computational power. On the other hand,
such constructs are often convenient. Design some iteration
constructs, give examples of their use, and show how
to implement them as derived expressions.

Solution:

@(codeblock (file->string "solutions/chapter4/ch4-ex09.rkt"))

@section{Exercise 4.10}

By using data abstraction, we were able to
write an eval procedure that is independent of the
particular syntax of the language to be evaluated. To illustrate this,
design and implement a new syntax for Scheme by modifying
the procedures in this section, without changing @tt{eval}
or @tt{apply}.

Solution:

See @secref{Exercise_4_3}