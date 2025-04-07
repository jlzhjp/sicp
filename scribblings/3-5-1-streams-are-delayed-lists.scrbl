#lang scribble/manual

@require[racket/file @for-label[racket]]

@title{Streams Are Delayed Lists}

@section{Exercise 3.50}
Complete the following definition, which generalizes @tt{stream-map} to allow procedures that take
multiple arguments, analogous to map in Section 2.2.1, Footnote 12.

@racketblock[
 (define (stream-map proc . argstreams)
   (if (⟨??⟩ (car argstreams))
       the-empty-stream
       (⟨??⟩
        (apply proc (map ⟨??⟩ argstreams))
        (apply stream-map
               (cons proc (map ⟨??⟩ argstreams))))))]

Solution:

@codeblock[@file->string{solutions/chapter3/ch3-ex50.rkt}]

@section{Exercise 3.51}
In order to take a closer look at delayed evaluation, we will use the following procedure, which
simply returns its argument after printing it:

@racketblock[
 (define (show x)
   (display-line x)
   x)]

What does the interpreter print in response to evaluating each expression in the following sequence?

@racketblock[
 (define x
   (stream-map show
               (stream-enumerate-interval 0 10)))
 (stream-ref x 5)
 (stream-ref x 7)]

Solution:

@codeblock[@file->string{solutions/chapter3/ch3-ex51.rkt}]

@section{Exercise 3.52}

Consider the sequence of expressions

@racketblock[
 (define sum 0)
 (define (accum x) (set! sum (+ x sum)) sum)
 (define seq
   (stream-map accum
               (stream-enumerate-interval 1 20)))
 (define y (stream-filter even? seq))
 (define z
   (stream-filter (lambda (x) (= (remainder x 5) 0))
                  seq))
 (stream-ref y 7)
 (display-stream z)]

What is the value of sum after each of the above expressions is evaluated? What is the printed
response to evaluating the @tt{stream-ref} and @tt{display-stream} expressions? Would these responses
differ if we had implemented @tt{(delay ⟨exp⟩)} simply as @tt{(lambda () ⟨exp⟩)} without using the
optimization provided by @tt{memo-proc?} Explain.

Solutions:

@codeblock[@file->string{solutions/chapter3/ch3-ex52.rkt}]
