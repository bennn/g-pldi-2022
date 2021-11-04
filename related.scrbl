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
@|sShallow| checks are the default and concrete data structures are available.

The model in @section-ref{sec:model} builds on the semantic framework
of @citet{gf-icfp-2018}, which is in turn inspired by
@citet{mf-toplas-2009}.
Unlike those frameworks, the present model uses a surface-to-evaluation compiler
similar to how @citet{clzv-ecoop-2018} compile several gradual languages to
the @exact{\kafka} core language.
Our particular compiler is inspired by @citet{h-scp-1994}, who defines
a completion pass to make run-time type checks explicit.

There is a great deal of related work that addresses the performance of
@|sdeep| or @|sshallow| types@~cite{bbst-oopsla-2017,kas-pldi-2019,vsc-dls-2019,rat-oopsla-2017,mt-oopsla-2017,rmhn-ecoop-2019,mt-oopsla-2021,mntv-popl-2021}.
This research is orthogonal; these implementation techniques should apply
to a three-way language as well as any normal gradual language.
Our language benefits from one such technique: collapsible contracts@~cite{fgsfs-oopsla-2018}.

@; similar acks for implementation (Sam, TR) and evaluation (Takikawa) ?

@; discuss other 3way ideas?

