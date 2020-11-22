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

@$${
\tt
\begin{array}{lc}

@mk-rule-name{E-App1}
  & {
      @t{1} \to @t{1}'
      \over
      {
        \left( @t{1}\ @t{2}^{\ast} \right)
        \to
        \left(@v{1}^{+}\ @t{2}'\ @t{3}^{\ast} \right)
      }
  } \\
  & \\

@mk-rule-name{E-App2}
  & {
      @t{2} \to @t{2}'
      \over
      {
        \left( @v{1}^{+}\ @t{2}\ @t{3}^{\ast} \right)
        \to
        \left( @v{1}^{+}\ @t{2}'\ @t{3}^{\ast} \right)
      }
  } \\
  & \\

@mk-rule-name{E-AppNull}
  & {
      \left( \left( \lambda \left(\ \right) @t{1} \right) \right)
      \to
      @t{1}
  } \\
  & \\

@mk-rule-name{E-AppUnary}
  & {
      \left( \left( \lambda \left( @(x) \right) @t{1} \right) @v{2} \right)
      \to
      @t{1}\left[@v{2} \middle/ @(x)\right]
  } \\
  & \\

@mk-rule-name{E-AppUncurry}
  & {
      {
        \left( @v{1}\ @v{2} \right)
        \to
        @t{12}
      }
      \over
      {
        \left( @v{1}\ @v{2}\ @v{3}^{+} \right)
        \to
        \left( @t{12}\ @v{3}^{+} \right)
      }
  } \\
  & \\

@mk-rule-name{E-AppVariadic}
  & {
      {
        \left( @t{1}\ @v{2} \right)
        \to
        @t{12}
      }
      \over
      {
        \left( \left( \lambda \left( @(x)\ \ldots \right) @t{1} \right) @v{2} \right)
        \to
        \left( \sigma\ @t{12} \left( \lambda \left( @(x)\ \ldots \right) @t{12} \right) \right)
      }
  } \\
  & \\

@mk-rule-name{E-AppSuper}
  & {
      {
        \left( @t{1}\ @v{3} \right) \to @v{4}
        \qquad
        \left( @t{2}\ @v{3} \right) \to @v{5}
      }
      \over
      {
        \left( \left( \sigma\ @t{1}\ @t{2} \right) @v{3} \right)
        \to
        \left( \sigma\ @v{4}\ @v{5} \right)
      }
  } \\
  & \\

@mk-rule-name{E-AppSuperErr1}
  & {
      {
        \left( @t{1}\ @v{3} \right) \to \epsilon
        \qquad
        \left( @t{2}\ @v{3} \right) \to @v{4}
      }
      \over
      {
        \left( \left( \sigma\ @t{1}\ @t{2} \right) @v{3} \right)
        \to
        @v{4}
      }
  } \\
  & \\

@mk-rule-name{E-AppSuperErr2}
  & {
      {
        \left( @t{1}\ @v{3} \right) \to @v{4}
        \qquad
        \left( @t{2}\ @v{3} \right) \to \epsilon
      }
      \over
      {
        \left( \left( \sigma\ @t{1}\ @t{2} \right) @v{3} \right)
        \to
        @v{4}
      }
  } \\
\end{array}
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.