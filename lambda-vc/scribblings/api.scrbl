#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[@for-label[lambda-vc
                    lambda-vc/base
                    (prefix-in racket/base: racket/base)
                    racket/contract]]

@title[#:tag "sec:api"]{λ@subscript{vc} API}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

@defmodule[lambda-vc]

@defproc[(interp [exp any/c] [env any/c]) value?]{
not used by @racket[value?]
}

@defproc[(value? [exp any/c]) boolean?]{
used by @racket[interp]
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.