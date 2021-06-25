#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt" scriblib/footnote)

@title[#:tag "sec:related"]{Related Work}

Four other gradual languages employ a combination of type enforcement
strategies.
By contrast to the multi-language approach of @|sDeep| and @|sShallow| Racket,
which allows two interpretations of one static type, the following introduce
new types to allow for additional interpretations.
@itemlist[
@item{
Thorn combines concrete types and erased types (@emph{like types}) according
to programmer-supplied annotations@~cite{wzlov-popl-2010}.
If a like type annotation is present, then Thorn does not enforce the type
at run-time.
}
@item{
StrongScript extends TypeScript@~cite{bat-ecoop-2014} with concrete types,
and thereby offers a similar combination to like types in which programmers may opt in to
run-time checking@~cite{rzv-ecoop-2015}.
}
@item{
Pyret employs @|sdeep| checks for fixed-size data and @|sshallow| checks for
everything else.@note{Personal communication. @shorturl["https://www.pyret.org" "pyret.org"]}
For example, pair types get a @|sdeep| check and function types get a
@|sshallow| check.
}
@item{
Static Python combines @|sshallow| and concrete checks.@note{Personal communication. @shorturl["https://github.com/facebookincubator/cinder" "github.com/facebookincubator/cinder"]}
@|sShallow| checks are the default.
Programmers can opt in to concrete by using one of several data structures
that the language provides.
}
]

The model in @section-ref{sec:model} builds on the semantic framework
of @citet{gf-icfp-2018}, which is in turn inspired by the multi-language
semantics of @citet{mf-toplas-2009}.
The model is also inspired by the @exact{\kafka} framework, in that it employs
three compilers to transform a declarative surface syntax to an evaluation
syntax that makes run-time checks manifest@~cite{clzv-ecoop-2018}.

@; similar acks for implementation (Sam, TR) and evaluation (Takikawa) ?

@; discuss other 3way ideas?

