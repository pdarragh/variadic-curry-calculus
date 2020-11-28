#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[@for-label[lambda-vc
                    lambda-vc/base
                    racket/contract]
         scribble-math
         "math-help.rkt"]
@(use-mathjax)

@title[#:tag "sec:semantics"]{λ@subscript{vc} Semantics}
@author[(@author+email "Pierce Darragh" "pierce.darragh@gmail.com")]

Some stuff.

@section{Syntax Rules}

@tabular[#:sep @hspace[1]
(list @; Terms Table Header
      (list @bold{t ::=} "" @bold{terms:} 'cont)
      @; Rows
      (list ""
            @tt{z}
            ""
            "integer")
      (list ""
            @tt{x}
            ""
            "variable")
      (list ""
            @tt{(λ (x@superscript{*}) t)}
            ""
            "n-ary function")
      (list ""
            @tt{(λ (x@superscript{+} ...) t)}
            ""
            "variadic function")
      (list ""
            @tt{σ(t)}
            ""
            "superposition")
      (list ""
            @tt{(t@superscript{+})}
            ""
            "application")
      @; Spacing
      (list "" "" "" "")
      @; Values Table Header
      (list @bold{v ::=} "" @bold{values:} 'cont)
      @; Rows
      (list ""
            @tt{z}
            ""
            "integer value")
      (list ""
            @tt{x}
            ""
            "variable value")
      (list ""
            @tt{(λ () t)}
            ""
            "nullary function value")
      (list ""
            @tt{(λ (x) t)}
            ""
            "unary function value")
      (list ""
            @tt{(λ (x ...) t)}
            ""
            "variadic function value")
      (list ""
            @tt{σ(t)}
            ""
            "superposition value"))]

@section{Evaluation Rules}

This is the operational semantics.

@tabular[#:sep @hspace[1] #:column-properties '(left center)
(list
  (list @$$rule-name{E-Substitute}
        @$${
          @empty
          \over
          @env{@x{1} -> @t{1}} @x{1}
          @evaluates-to
          @env{} @t{1}
        })
  (list @$$rule-name{E-Curry}
        @$${
          @empty
          \over
          @func{@x{1} @space @x{2}^{+} . @t{1}}
          @evaluates-to
          @func{@x{1} . @func{@x{2}^{+} . @t{1}}}
        })
  (list @$$rule-name{E-AppReduce}
        @$${
          @t{1} @evaluates-to @t{1}'
          \over
          {
            @parens{@t{1} @space @t{2}^{\ast}}
            @evaluates-to
            @parens{@t{1}' @space @t{2}^{\ast}}
          }
        })
  (list @$$rule-name{E-AppNull}
        @$${
          @t{1} @evaluates-to @t{1}'
          \over
          {
            @parens{@func{@space . @t{1}}}
            @evaluates-to
            @t{1}'
          }
        })
  (list @$$rule-name{E-AppUnary}
        @$${
          @env{@x{1} -> @t{2}} @t{1}
          @evaluates-to
          @t{1}'
          \over
          @(env) @parens{@func{@x{1} . @t{1}} @space @t{2}}
          @evaluates-to
          @t{1}'
        })
  (list @$$rule-name{E-AppUncurry}
        @$${
          @empty
          \over
          {
            @parens{@v{1} @space @t{2} @space @t{3}^{+}}
            @evaluates-to
            @parens{@parens{@v{1} @space @t{2}} @space @t{3}^{+}}
          }
        })
  (list @$$rule-name{E-AppVariadic}
        @$${
          {
            @parens{@t{1} @space @v{2}}
            @evaluates-to
            @t{12}
          }
          \over
          {
            @env{@x{1} -> @t{1}} @parens{@func{@x{1} @space @ellipsis . @t{1}} @space @v{2}}
            @evaluates-to
            @sup{@t{12} | @func{@x{1} @space @ellipsis . @t{12}}}
          }
        })
  (list @$$rule-name{E-AppSuper}
        @$${
          {
            @t{3}
            @evaluates-to
            @t{3}'

            \qquad

            @parens{@t{1} @space @t{3}'}
            @evaluates-to
            @t{13}

            \qquad

            @parens{@t{2} @space @t{3}'}
            @evaluates-to
            @t{23}
          }
          \over
          {
            @parens{@sup{@t{1} | @t{2}} @space @t{3}}
            @evaluates-to
            @sup{@t{13} | @t{23}}
          }
        })
  (list @$$rule-name{E-AppSuperErr1}
        @$${
          {
            @t{3}
            @evaluates-to
            @t{3}'

            \qquad

            @parens{@t{1} @space @t{3}'}
            @evaluates-to
            @err

            \qquad

            @parens{@t{2} @space @t{3}'}
            @evaluates-to
            @t{23}
          }
          \over
          {
            @parens{@sup{@t{1} | @t{2}} @space @t{3}}
            @evaluates-to
            @t{23}
          }
        })
  (list @$$rule-name{E-AppSuperErr2}
        @$${
          {
            @t{3}
            @evaluates-to
            @t{3}'

            \qquad

            @parens{@t{1} @space @t{3}'}
            @evaluates-to
            @t{13}

            \qquad

            @parens{@t{2} @space @t{3}'}
            @evaluates-to
            @err
          }
          \over
          {
            @parens{@sup{@t{1} | @t{2}} @space @t{3}}
            @evaluates-to
            @t{13}
          }
        }))]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.