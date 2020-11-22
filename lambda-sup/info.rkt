#lang info

(define name "lambda-sup")
(define collection "lambda-sup")
(define version "0.0.1")

(define pkg-desc "A small, untyped lambda calculus extended to support both automatic currying and variadic functions.")
(define pkg-authors '((@author+email "Pierce Darragh" "pierce.darragh@gmail.com")))

(define scribblings '(("scribblings/lambda-sup.scrbl" (multi-page main-doc) (library))))

(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
