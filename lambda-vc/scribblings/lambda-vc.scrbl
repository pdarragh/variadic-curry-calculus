#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scriblib/footnote]

@title{λ@subscript{vc}: Variadic Auto-Curried Lambda Calculus}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

This package implements λ@subscript{vc}: the lambda calculus extended to
support @bold{v}ariadic functions and automatic @bold{c}urrying of multary
functions.
The code lives on GitHub at
@hyperlink["https://github.com/pdarragh/variadic-curry-calculus"]{@tt{pdarragh/variadic-curry-calculus}}.

@;
@table-of-contents[]
@include-section["background.scrbl"]
@include-section["syntax.scrbl"]
@include-section["semantics.scrbl"]
@include-section["using-interpreter.scrbl"]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
