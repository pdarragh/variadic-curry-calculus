#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[@for-label[lambda-vc/base
                    racket/contract]]

@title[#:tag "sec:api"]{λ@subscript{vc} API}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

@defmodule[lambda-vc]

@defproc[(interp [exp any/c] [env (listof pair?)]) value?]{
Function function function.
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.