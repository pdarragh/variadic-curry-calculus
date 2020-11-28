#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scriblib/footnote]

@title{λ@subscript{vc}: Variadic Auto-Curried Lambda Calculus}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

This package implements λ@subscript{vc}: the lambda calculus extended to
support @bold{v}ariadic functions and automatic @bold{c}urrying of multary
functions.

In a sane lambda calculus implementation, variadic functions and auto-currying
are at odds.
This is because a variadic functions says "I take any number of arguments in my
last parameter" while automatic currying says "Functions may only take one
argument at a time."
When you apply a variadic function to more than one value, a question is
raised: is the second value an additional variadic argument, or is it meant to
be used in the application of the @italic{result} of the first application?

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
When a superposition is applied to a value, the value is applied to both the
result of the variadic function that begat the superposition as well as the
variadic function itself as though it were waiting for an additional argument.

To make this work, variadic functions @italic{must} be declared within the
context of another non-nullary function.
When a variadic function is applied, we evaluate its body with the proper
substitution as though it were a regular unary function.
However, we also construct a new version of the variadic function with an
updated environment.
In this environment, the value bound during application of the parent function
is replaced with the result of the variadic application.

This is an implementation of a small, untyped lambda calculus with support for
both automated currying and variadic functions.
@;
@table-of-contents[]
@include-section["api.scrbl"]
@include-section["semantics.scrbl"]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.