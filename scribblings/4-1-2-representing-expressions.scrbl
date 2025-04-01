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
@codeblock{
(let ((<var₁> <exp₁>) ... (<varₙ> <expₙ>))
  <body>)
}

is equivalent to
@codeblock{
((lambda (<var₁> ... <varₙ>)
   <body>)
 <exp₁>
 ...
 <expₙ>)
}

Implement a syntactic transformation @tt{let->combination} that reduces @tt{let}
expressions to procedure calls as shown, and add the appropriate clause to
@tt{eval} to handle @tt{let} expressions.

@(codeblock (file->string "solutions/chapter4/ch4-ex06.rkt"))