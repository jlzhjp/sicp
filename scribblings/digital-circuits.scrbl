#lang scribble/manual

@title[#:tag "digital-circuits"]{A Simulator for Digital Circuits}
@(require racket/file
          (for-label racket
                     akari-sicp/lib/testing
                     akari-sicp/lib/digital-circuits))

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

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex29.rkt"))

@section{Exercise 3.30}

@italic{ripple-carry adder} is the simplest form of parallel adder for
adding two @italic{n}-bit binary numbers. The inputs @italic{A₁, A₂, A₃, …, Aₙ} and
@italic{B₁, B₂, B₃, …, Bₙ} are the two binary numbers to be added (each @italic{Aₖ} and
@italic{Bₖ} is a 0 or a 1). The circuit generates @italic{S₁, S₂, S₃, …, Sₙ},
the @italic{n} bits of the sum, and @italic{C}, the carry from the addition.

Write a procedure @tt{ripple-carry-adder} that generates this circuit.
The procedure should take as arguments three lists of @italic{n} wires each —
the @italic{Aₖ}, the @italic{Bₖ}, and the @italic{Sₖ} — and also another wire @italic{C}.

The major drawback of the ripple-carry adder is the need to wait for the carry signals
to propagate. What is the delay needed to obtain the complete output from
an @italic{n}-bit ripple-carry adder, expressed in terms of the delays
for @tt{and}-gates, @tt{or}-gates, and @tt{inverters}?

Solution:

@(codeblock (file->string "solutions/chapter3/ch3-ex30.rkt"))
