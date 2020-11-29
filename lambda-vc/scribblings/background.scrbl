#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scriblib/footnote]

@title[#:tag "sec:background"]{Background}

On November 18, 2020, Twitter user
@hyperlink["https://twitter.com/fogus"]{@"@"fogus} sent out
@hyperlink["https://twitter.com/fogus/status/1329097890359349248?s=20"]{a
tweet} asking:

@nested[#:style 'inset]{@italic{If you had a Lisp with auto-currying what would
you expect as the application of ((- 1) 2)?}}

Traditionally, the Lisp arithmetic functions are variadic.
This means they can be given any number of arguments and compute a result:

@codeblock|{
  > (+ 1)
  1
  > (+ 1 2)
  3
  > (+ 1 2 3)
  6
}|

Meanwhile, auto-currying refers to a language feature where functions that
accept multiple arguments are converted into sets of nested functions that each
accept only @italic{one} argument, with the intermediate functions returning
functions themselves.
In a Lisp-like lambda calculus notation, this might look like:

@codeblock|{
  ;; We put in a function that takes three arguments...
  > (λ (x1 x2 x3) t)
  ;; And we get back a nesting of three single-argument functions.
  (λ (x1) (λ (x2) (λ (x3) t)))
}|

This means that, in a sane programming language, variadic functions and
auto-currying are at odds.
A variadic functions says "I take any number of arguments in my last parameter"
while automatic currying says "Functions may only take one argument at a time."
When you apply a variadic function to more than one value in a language with
automatic currying, a question is raised: is the second value an additional
variadic argument, or is it meant to be used in the application of the
@italic{result} of the first application?

To resolve this truly nefarious problem, λ@subscript{vc} implements what I call
@italic{superpositions}, a term borrowed from quantum physics.
In physics, the superposition is the notion that a system may exist in multiple
distinct states simultaneously until it is observed.
Generally, the states each have a distinct probability, and this probability
plays a role in determining which state will remain when the system is
observed.

In λ@subscript{vc}, the superposition encodes two states@note{
  Either of those states can itself be a superposition, so representing a
  superposition of more than two states is possible by nesting.
} that are considered "equally likely" (in that no probability is assigned).
Observation occurs through application.
When a superposition is applied to a value, application distributes across both
branches of the superposition: the result of the variadic function that begat
the superposition as well as the variadic function itself as though it were
waiting for an additional argument.

To make this work, variadic functions @italic{must} be declared within the
context of another non-nullary function.
When a variadic function is applied, we evaluate its body with the proper
substitution as though it were a regular unary function.
However, we also construct a new version of the variadic function with an
updated environment.
In this environment, the value bound during application of the parent function
is replaced with the result of the variadic application.
This is kept around in the variadic closure for subsequent applications (if
any).

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
