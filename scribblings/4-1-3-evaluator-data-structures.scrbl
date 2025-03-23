#lang scribble/manual

@(require racket/file
          (for-label racket))

@title{Evaluator Data Structures}

@section{Exercise 4.11}

@para{Instead of representing a frame as a pair of lists, we can represent a
 frame as a list of bindings, where each binding is a name-value pair. Rewrite
 the environment operations to use this alternative representation.}

@(codeblock (file->string "solutions/chapter4/ch4-ex11.rkt"))


@section{Exercise 4.12}

@para{The procedures @tt{define-variable!}, @tt{set-variable-value!}, and
 @tt{lookup-variable-value} can be expressed in terms of more abstract
 procedures for traversing the environment structure. Define abstractions that
 capture the common patterns and redefine the three procedures in terms of these
 abstractions.}

@(codeblock (file->string "solutions/chapter4/ch4-ex12.rkt"))