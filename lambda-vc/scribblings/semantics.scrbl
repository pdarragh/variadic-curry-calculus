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
        @$$judgment[
          @env{@x{1} -> @t{1}} @x{1}
          @evaluates-to
          @env{} @t{1}
        ])
  (list @$$rule-name{E-Curry}
        @$$judgment[
          @func{@x{1} @space @x{2}^{+} . @t{1}}
          @evaluates-to
          @func{@x{1} . @func{@x{2}^{+} . @t{1}}}
        ])
  (list @$$rule-name{E-AppReduce}
        @$$judgment[
          #:premise @group{
                      @t{1}
                      @evaluates-to
                      @t{1}'
          }
          #:conclusion @group{
                        @parens{@t{1} @space @t{2}^{\ast}}
                        @evaluates-to
                        @parens{@t{1}' @space @t{2}^{\ast}}
          }])
  (list @$$rule-name{E-AppNull}
        @$$judgment[
          #:premise @group{
                      @t{1}
                      @evaluates-to
                      @t{1}'
          }
          #:conclusion @group{
                        @parens{@func{@space . @t{1}}}
                        @evaluates-to
                        @t{1}'
          }])
  (list @$$rule-name{E-AppUnary}
        @$$judgment[
          #:premise @group{
                      @env{@x{1} -> @t{2}} @t{1}
                      @evaluates-to
                      @t{1}'
          }
          #:conclusion @group{
                        @(env) @parens{@func{@x{1} . @t{1}} @space @t{2}}
                        @evaluates-to
                        @t{1}'
          }])
  (list @$$rule-name{E-AppUncurry}
        @$$judgment[
          @parens{@v{1} @space @t{2} @space @t{3}^{+}}
          @evaluates-to
          @parens{@parens{@v{1} @space @t{2}} @space @t{3}^{+}}
        ])
  (list @$$rule-name{E-AppVariadic}
        @$$judgment[
          #:premise @group{
                      @parens{@t{1} @space @v{2}}
                      @evaluates-to
                      @t{12}
          }
          #:conclusion @group{
                        @env{@x{1} -> @t{1}} @parens{@func{@x{1} @space @ellipsis . @t{1}} @space @v{2}}
                        @evaluates-to
                        @sup{@t{12} | @func{@x{1} @space @ellipsis . @t{12}}}
          }])
  (list @$$rule-name{E-AppSuper}
        @$$judgment[
          #:premises (list
                      @group{
                        @t{3}
                        @evaluates-to
                        @t{3}'}
                      @group{
                        @parens{@t{1} @space @t{3}'}
                        @evaluates-to
                        @t{13}}
                      @group{
                        @parens{@t{2} @space @t{3}'}
                        @evaluates-to
                        @t{23}})
          #:conclusion @group{
                        @parens{@sup{@t{1} | @t{2}} @space @t{3}}
                        @evaluates-to
                        @sup{@t{13} | @t{23}}
          }])
  (list @$$rule-name{E-AppSuperErr1}
        @$$judgment[
          #:premises (list
                      @group{
                        @t{3}
                        @evaluates-to
                        @t{3}'}
                      @group{
                        @parens{@t{1} @space @t{3}'}
                        @evaluates-to
                        @err}
                      @group{
                        @parens{@t{2} @space @t{3}'}
                        @evaluates-to
                        @t{23}})
          #:conclusion @group{
                        @parens{@sup{@t{1} | @t{2}} @space @t{3}}
                        @evaluates-to
                        @t{23}
          }])
  (list @$$rule-name{E-AppSuperErr2}
        @$$judgment[
          #:premises (list
                      @group{
                        @t{3}
                        @evaluates-to
                        @t{3}'}
                      @group{
                        @parens{@t{1} @space @t{3}'}
                        @evaluates-to
                        @t{13}}
                      @group{
                        @parens{@t{2} @space @t{3}'}
                        @evaluates-to
                        @err})
          #:conclusion @group{
                        @parens{@sup{@t{1} | @t{2}} @space @t{3}}
                        @evaluates-to
                        @t{13}
          }]))]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.