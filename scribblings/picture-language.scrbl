#lang scribble/manual

@(require scribble/example
          racket/file
          (for-label racket/base
                     akari-sicp/solutions/chapter2/ch2-ex44
                     akari-sicp/solutions/chapter2/ch2-ex45
                     akari-sicp/solutions/chapter2/ch2-ex49
                     akari-sicp/solutions/chapter2/ch2-ex50
                     akari-sicp/solutions/chapter2/ch2-ex51
                     akari-sicp/lib/picture))

@(define the-evaluator (make-base-eval))

@(the-evaluator
  '(require (only-in akari-sicp/solutions/chapter2/ch2-ex44 square-limit)
            (only-in akari-sicp/solutions/chapter2/ch2-ex45 up-split right-split)
            (only-in akari-sicp/solutions/chapter2/ch2-ex49 outline x diamond wave)
            (only-in akari-sicp/solutions/chapter2/ch2-ex50 flip-vert flip-horiz
                     squash-inwards shrink-to-upper-right rotate90 rotate180 rotate270)
            (only-in akari-sicp/solutions/chapter2/ch2-ex51 beside below below*)
            (only-in akari-sicp/lib/picture painter->image rogers)))

@title{Example: A Picture Language}


@section{Exercise 2.44}

Define the procedure @racket[up-split] used by @racket[corner-split].
It is similar to @racket[right-split], except that it switches
the roles of below and beside.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex44.rkt"))

@examples[#:eval the-evaluator (painter->image '(256 256) (square-limit wave 4))]

@examples[#:eval the-evaluator (painter->image '(320 388) (square-limit rogers 4))]


@section{Exercise 2.45}

@tt{right-split} and @tt{up-split} can be expressed
as instances of a general splitting operation. Define a
procedure @tt{split} with the property that evaluating

@codeblock{
 (define right-split (split beside below))
 (define up-split (split below beside))
}

produces procedures @tt{right-split} and @tt{up-split} with the
same behaviors as the ones already defined.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex45.rkt"))

@examples[#:eval the-evaluator (painter->image '(128 64) (right-split wave 4))]

@examples[#:eval the-evaluator (painter->image '(64 128) (up-split wave 4))]

@section[#:tag "ex2-46"]{Exercise 2.46}

Implement a data abstraction for vectors by giving a constructor @tt{make-vect}
and corresponding selectors @tt{xcor-vect} and @tt{ycor-vect.} In
terms of your selectors and constructor, implement procedures
@tt{add-vect}, @tt{sub-vect}, and @tt{scale-vect} that perform
the operations vector addition, vector subtraction, and multiplying a vector by a scalar

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex46.rkt"))


@section{Exercise 2.47}

Here are two possible constructors for frames:

@typeset-code{
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
}

For each constructor supply the appropriate selectors to
produce an implementation for frames.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex47.rkt"))


@section{Exercise 2.48}

A directed line segment in the plane can be
represented as a pair of vectors—the vector running from
the origin to the start-point of the segment, and the vector
running from the origin to the end-point of the segment.

Use your vector representation from @secref["ex2-46"] to define
a representation for segments with a constructor @tt{make-segment}
and selectors @tt{start-segment} and @tt{end-segment}.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex48.rkt"))


@section{Exercise 2.49}

Use @racket[segments->painter] to define the following primitive painters:

@itemlist[#:style 'ordered
  @item{The painter that draws the outline of the designated frame.}
  @item{The painter that draws an “X” by connecting opposite corners of the frame.}
  @item{The painter that draws a diamond shape by connecting the midpoints of the sides
   of the frame.}
  @item{The wave painter.}
]

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex49.rkt"))

@examples[#:eval the-evaluator (painter->image '(64 64) outline)]

@examples[#:eval the-evaluator (painter->image '(64 64) x)]

@examples[#:eval the-evaluator (painter->image '(64 64) diamond)]

@examples[#:eval the-evaluator (painter->image '(64 64) wave)]


@section{Exercise 2.50}

Define the transformation @tt{flip-horiz}, which
flips painters horizontally, and transformations that rotate
painters counterclockwise by 180 degrees and 270 degrees.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex50.rkt"))

@examples[#:eval the-evaluator (painter->image '(64 64) (flip-vert rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (flip-horiz rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (squash-inwards rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (shrink-to-upper-right rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (rotate90 rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (rotate180 rogers))]

@examples[#:eval the-evaluator (painter->image '(64 64) (rotate270 rogers))]


@section{Exercise 2.51}

Define the below operation for painters. below
takes two painters as arguments. e resulting painter, given
a frame, draws with the first painter in the boom of the
frame and with the second painter in the top. Define below
in two different ways—first by writing a procedure that is
analogous to the beside procedure given above, and again
in terms of beside and suitable rotation operations.

Solution:

@(codeblock (file->string "solutions/chapter2/ch2-ex51.rkt"))

@examples[#:eval the-evaluator (painter->image '(128 64) (beside wave rogers))]

@examples[#:eval the-evaluator (painter->image '(64 128) (below wave rogers))]

@examples[#:eval the-evaluator (painter->image '(64 128) (below* wave rogers))]

@(close-eval the-evaluator)
