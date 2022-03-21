#lang scribble/acmart
@(require "main.rkt" "bib.rkt" scriblib/footnote)

@title[#:tag "sec:related"]{Related Work}

Two gradual languages, Thorn@~cite{wzlov-popl-2010} and
StrongScript@~cite{rzv-ecoop-2015}, support a combination of sound
@emph{concrete} types and erased @emph{like} types.
Thorn is a scalable scripting language that compiles to the JVM@~cite{bfnorsvw-oopsla-2009}.
StrongScript extends TypeScript@~cite{bat-ecoop-2014} with concrete types.
Pyret explores a type-based combination, with @|sdeep| checks for types that
describe fixed-size data and @|sshallow| checks for other types.@note{Personal
communication.  @shorturl["https://www.pyret.org" "pyret.org"]}
For example, pair types get a @|sdeep| check and list types get a @|sshallow|
check.
Static Python combines @|sshallow| and concrete checks@~cite{lgmvpk-draft-2022}.
@|sShallow| checks are the default, and programmers can opt-in to concrete
data structures.
Outside the realm of gradual typing, option contracts allow client code to trust
(and skip checking) specific contracts from server code@~cite{dff-oopsla-2013}.

The model in @section-ref{sec:model} builds on the semantic framework
of @citet{gf-icfp-2018}, which is in turn inspired by
@citet{mf-toplas-2009}.
Unlike those frameworks, the present model uses a surface-to-evaluation compiler
similar to how @citet{clzv-ecoop-2018} compile several gradual languages to
the @exact{\kafka} core language.
The compiler in @section-ref{sec:model} is inspired by the coercion calculus@~cite{h-scp-1994};
in particular, its @emph{completion} pass that makes run-time type checks explicit.

There is a great deal of related work that addresses the performance of
@|sdeep| or @|sshallow| types via implementation techniques@~cite{fgsfs-oopsla-2018},
static analysis@~cite{ngtv-popl-2018,ngtv-pldi-2019,vsc-dls-2019,mntv-popl-2021},
compilation techniques@~cite{bbst-oopsla-2017,rat-oopsla-2017,rmhn-ecoop-2019},
and clean-slate language designs@~cite{mt-oopsla-2017,kas-pldi-2019,mt-oopsla-2021}.
These improvements are orthogonal to a combined language; they should
apply to a three-way language as well as any normal gradual language.
As a case in point, our three-way Typed Racket
benefits from collapsible contracts@~cite{fgsfs-oopsla-2018}.

@; similar acks for implementation (Sam, TR) and evaluation (Takikawa) ?

@; discuss other 3way ideas?

