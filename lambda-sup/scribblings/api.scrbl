#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[@for-label[lambda-sup/base
                    racket/contract]]

@title[#:tag "sec:api"]{Lambda-Sup API}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

@defmodule[lambda-sup]

@defproc[(interp [exp any/c] [env (listof pair?)]) value?]{
Function function function.
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.