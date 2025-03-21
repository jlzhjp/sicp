#lang scribble/manual

@(require (only-in akari-sicp/solutions/chapter2/ch2-ex44 square-limit)
          (only-in akari-sicp/solutions/chapter2/ch2-ex49 wave)
          (only-in akari-sicp/lib/picture with-drawing-to-image
                                          rogers
                                          frame-whole-canvas))

@title{Example: A Picture Language}

@section{Exercise 2.44}

Define the procedure @racket[up-split] used by @racket[corner-split].
It is similar to @racket[right-split], except that it switches
the roles of below and beside.

@(with-drawing-to-image '(512 512)
    (lambda ()
      ((square-limit wave 4) frame-whole-canvas)))

@(with-drawing-to-image '(640 776)
    (lambda ()
      ((square-limit rogers 4) frame-whole-canvas)))