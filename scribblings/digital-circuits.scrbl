#lang scribble/manual

@title[#:tag "digital-circuits"]{A Simulator for Digital Circuits}
@(require (for-label racket
                     akari-sicp/lib/testing
                     akari-sicp/lib/digital-circuits))
@(require racket/file)

@section{Exercise 3.28}
Define an @racket[or-gate] as a primitive function box.
Your @racket[or-gate] constructor should be similar to @racket[and-gate]

@(codeblock (file->string "solutions/chapter3/ch3-ex28.rkt"))

@section{Exercise 3.29}
Another way to construct an @racket[or-gate] is as
a compound digital logic device, built from and-gates and
inverters. Define a procedure or-gate that accomplishes
this. What is the delay time of the @racket[or-gate] in terms of
@racket[and-gate-delay] and @racket[inverter-delay]?

@subsubsub*section{Solution}

@(codeblock (file->string "solutions/chapter3/ch3-ex29.rkt"))