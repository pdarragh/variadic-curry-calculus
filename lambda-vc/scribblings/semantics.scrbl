#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/bnf
         @for-label[lambda-vc
                    lambda-vc/base
                    racket/contract]]

@title[#:tag "sec:semantics"]{λ@subscript{vc} Semantics}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

Some stuff.

@(let ([lpar @litchar{(}]
       [rpar @litchar{)}]
       [lam @litchar{λ}]
       [ell @litchar{...}])
   @BNF[(list @nonterm{t}
              @nonterm{z}
              @nonterm{x}
              @BNF-seq[lpar lam lpar rpar @nonterm{t} rpar])])

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.