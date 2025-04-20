#lang scribble/manual

@(require (for-label racket)
          racket/file)

@title{Generic Arithmetic Operations}

@section{Exercise 2.77}
Louis Reasoner tries to evaluate the expression @racket[(magnitude z)] where @racket[z] is the object
shown in Figure 2.24. To his surprise, instead of the answer 5 he gets an error message from
@racket[apply-generic], saying there is no method for the operation @racket[magnitude] on the types
@racket[(complex)]. He shows this interaction to Alyssa P. Hacker, who says "The problem is that the
complex-number selectors were never defined for complex numbers, just for polar and rectangular
numbers. All you have to do to make this work is add the following to the complex package:"

@racketblock[
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
]

Describe in detail why this works. As an example, trace through all the procedures called in
evaluating the expression @racket[(magnitude z)] where @racket[z] is the object shown in
Figure 2.24. In particular, how many times is @racket[apply-generic] invoked? What procedure
is dispatched to in each case?

@codeblock[@(file->string "solutions/chapter2/ch2-ex77.rkt")]

@section{Exercise 2.78}
The internal procedures in the scheme-number package are essentially nothing more than calls to the
primitive procedures @racket[+], @racket[-], etc. It was not possible to use the primitives of the
language directly because our type-tag system requires that each data object have a type attached
to it. In fact, however, all Lisp implementations do have a type system, which they use internally.
Primitive predicates such as @racket[symbol?] and @racket[number?] determine whether data objects
have particular types. Modify the definitions of @racket[type-tag], @racket[contents], and
@racket[attach-tag] from 2.4.2 so that our generic system takes advantage of Scheme's internal
type system. That is to say, the system should work as before except that ordinary numbers should
be represented simply as Scheme numbers rather than as pairs whose @racket[car] is the symbol
@tt{scheme-number}.

@codeblock[@(file->string "solutions/chapter2/ch2-ex78.rkt")]

@section{Exercise 2.79}
Define a generic equality predicate @racket[equ?] that tests the equality of two numbers, and
install it in the generic arithmetic package. This operation should work for ordinary numbers,
rational numbers, and complex numbers.

@codeblock[@(file->string "solutions/chapter2/ch2-ex79.rkt")]

@section{Exercise 2.80}
Define a generic predicate @racket[=zero?] that tests if its argument is zero, and install it in the
generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and
complex numbers.

@codeblock[@(file->string "solutions/chapter2/ch2-ex80.rkt")]