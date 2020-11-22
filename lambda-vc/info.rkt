#lang info

(define name "lambda-vc")
(define collection "lambda-vc")
(define version "0.0.1")

(define pkg-desc "A small, untyped lambda calculus extended to support both automatic currying and variadic functions.")
(define pkg-authors '((@author+email "Pierce Darragh" "pierce.darragh@gmail.com")))

(define scribblings '(("scribblings/lambda-vc.scrbl" (multi-page main-doc) (library))))

(define deps '("base"
               "scribble-math"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
