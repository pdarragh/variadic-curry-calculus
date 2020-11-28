#lang scribble/manual
@; -*- mode: Scribble -*-

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@title[#:tag "sec:syntax"]{Syntax}

This section defines the valid syntax that can be used in constructing terms in
λ@subscript{vc}.
A superscript @tt{@superscript{*}} indicates a repetition of the preceding item
zero or more times.
The superscript @tt{@superscript{+}} indicates a repetition of the preceding item
one or more times.
The lambda @tt{λ}, sigma @tt{σ}, ellipsis @tt{...}, and parentheses @tt{()} are
all literals.

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
            @tt{(σ t t)}
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
            @tt{(σ v v)}
            ""
            "superposition value"))]
