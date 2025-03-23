#lang scribble/manual

@(require racket/file
          (for-label racket))

@title{The Core of the Evaluator}

@section{Exercise 4.1}

Notice that we cannot tell whether the metacircular evaluator evaluates operands
from left to right or from right to left. Its evaluation order is inherited from
the underlying Lisp: If the arguments to cons in @tt{list-of-values} are evaluated
from left to right, then @tt{list-of-values} will evaluate operands from left to
right; and if the arguments to cons are evaluated from right to left, then
@tt{list-of-values} will evaluate operands from right to left.

Write a version of @tt{list-of-values} that evaluates operands from left to right
regardless of the order of evaluation in the underlying Lisp. Also write a
version of @tt{list-of-values} that evaluates operands from right to left.

Solution:

@(codeblock (file->string "solutions/chapter4/ch4-ex01.rkt"))
