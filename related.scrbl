#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt" scriblib/footnote)

@title[#:tag "sec:related"]{Related Work}

Thorn@~cite{wzlov-popl-2010} and StrongScript@~cite{rzv-ecoop-2015} support
a combination of sound concrete types and erased @emph{like} types.
Thorn is a scalable scripting language that compiles to the JVM@~cite{bfnorsvw-oopsla-2009}.
StrongScript extends TypeScript@~cite{bat-ecoop-2014} with concrete types.
Both languages are supported by formal models with proofs of type soundness.

Pyret uses @|sdeep| checks for fixed-size data and @|sshallow| checks for
other data.@note{Personal communication. @shorturl["https://www.pyret.org" "pyret.org"]}
For example, pair types get a @|sdeep| check and function types get a
@|sshallow| check.
Static Python combines @|sshallow| and concrete checks.@note{Personal communication. @shorturl["https://github.com/facebookincubator/cinder" "github.com/facebookincubator/cinder"]}
@|sShallow| checks are the default; concrete data structures are available.

The model in @section-ref{sec:model} builds on the semantic framework
of @citet{gf-icfp-2018}, which is in turn inspired by
@citet{mf-toplas-2009}.
The model is also inspired by the @exact{\kafka} framework, which introduces
four compilers to transform a declarative surface syntax to an evaluation
syntax that makes run-time checks manifest@~cite{clzv-ecoop-2018}.

@; similar acks for implementation (Sam, TR) and evaluation (Takikawa) ?

@; discuss other 3way ideas?

