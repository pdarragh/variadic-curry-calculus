#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@title[#:tag "sec:interpeter"]{Using the Reference Interpreter}

This package provides a reference interpreter for λ@subscript{vc}.
After installing this package, you can @racket[(require lambda-vc)].
Only one function is exposed: @racket[interp].
The recommended way to interact with this function is through quotation:

@codeblock|{
  > (require lambda-vc)
  > (interp `((λ (x) x) 42))
  42
}|

The reference interpreter will accept @tt{lambda} in place of @tt{λ}.
The exposed @racket[interp] function operates in a big-step style, meaning it
eagerly evaluates results until it gets either a value or an error.
There may come a day when I change the exposed API to provide both small- and
big-step interpreters, but today is not that day.

In addition, the reference interpreter provides a few arithmetic functions that
can be used: @racket[+], @racket[-], @racket[*], and @racket[/].
Using these, let's explore variadic functions:

@codeblock|{
  ;; First, let's see what happens when we interpret a variadic function.
  > (interp `(λ (a b ...) (+ a b)))
  (λ (a) (λ (b ...) (+ a b)))
  ;; Now, let's apply it to a value.
  > (interp `((λ (a b ...) (+ a b)) 1))
  (λ (b ...) (+ a b))
  ;; λvc does not substitute eagerly, so this is correct.
  ;; Now, let's apply a second value.
  > (interp `((λ (a b ...) (+ a b)) 1 2))
  (σ 3 (λ (b ...) (+ a b)))
  ;; We can apply the result a few more times...
  > (interp `((λ (a b ...) (+ a b)) 1 2 3 4 5))
  (σ 15 (λ (b ...) (+ a b)))
}|

Each time we apply the variadic function, the resulting value is substituted in
for the @tt{a} that was previously bound to the preceding value.
As a result, we get a superposition which represents the fact that we don't
know whether we are done applying the function or if it should continue to
accept additional arguments.
