#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@title{λ@subscript{vc}: Variadic Auto-Curried Lambda Calculus}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

This is an implementation of a small, untyped lambda calculus with support for
both automated currying and variadic functions.
@;
@table-of-contents[]
@include-section["api.scrbl"]
@include-section["semantics.scrbl"]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.