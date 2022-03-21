#lang scribble/acmart
@(require "main.rkt" "bib.rkt"
   (only-in scriblib/footnote note)
   (only-in "pict.rkt"
     fig:model-interaction-y
     fig:ds-example-y))

@; THESIS Natural and Transient are the only realistic options
@;  - more imagined ideas in JFP submission
@;  - side benefit, explore two extremes (wrappers / none)

@; - don't justify TR
@; - keep this short, get to the model asap

@title[#:tag "sec:background"]{Background}

@section{Gradual, Migratory, Mixed-Typed}

Gradual typing explores combinations of static and dynamic
typing@~cite{st-sfp-2006,tf-dls-2006,mf-toplas-2009,gktff-sfp-2006}.
The goal of this research is a language that supports two styles of code in a
convenient manner.
Untyped code is free to perform any computation that the language can express.
Typed code is restricted to well-typed computations, but comes
with a guarantee that static types are meaningful predictions about run-time
behaviors.
Differences among gradual languages arise
over what makes for a convenient mix.
True gradually-typed languages include a universal @tt{Dynamic} type that
helps to blur the distinction between typed and untyped code@~cite{svcb-snapl-2015}.
Migratory typing systems add
idiomatic types to an existing language@~cite{tfffgksst-snapl-2017}.
Other mixed-typed methods include the development of
novel
languages@~cite{wzlov-popl-2010,mt-oopsla-2017,kas-pldi-2019,rmhn-ecoop-2019}
and compilers@~cite{rat-oopsla-2017,bbst-oopsla-2017}.

With these various end-goals in mind, our formal development
(@section-ref{sec:model}) begins with two restrictions:
types may only be enforced with ahead-of-time techniques and
there is no dynamic type.
These rules ensure a widely-applicable baseline for languages
that can mix typed and untyped code.


@;  @note{Following @citet{svcb-snapl-2015},
@;  we reserve the name @emph{gradual} for mixed-typed languages with
@;  a dynamic type that satisfies the gradual guarantees.})
@;  @; graduality@~cite{nla-popl-2019}


@section[#:tag "sec:background:deep-shallow"]{@|sDeep| and @|sShallow| Types}

@(define untyped-fn @tt{text})

Sound gradual language designs do not agree on how types should guide the
behavior of a program.
Two leading alternatives for run-time properties are @|sdeep| and @|sshallow| types.
To a first approximation, @|sdeep| types enforce (but do not verify) the same
guarantees as conventional static types and @|sshallow| types enforce only
local type soundness.

@Figure-ref{fig:ds-example} presents a three-module program to illustrate
the gap between @|sdeep| and @|sshallow| types.
The untyped module on top contains a stub definition for a function
@|untyped-fn| that expects two arguments.
This module is a simplified picture of the Racket @tt{images/icons/symbol}
module, which incorporates thousands of lines of rendering and raytracing code---a
module that is easiest left untyped.
The typed module in the middle is an interface for the untyped function,
which passes on (in a higher-order manner) to clients who might rely on the type.
The type correctly states that @|untyped-fn| expects a string and a font
object and computes a bitmap object.
Finally, the untyped client module on the bottom mistakenly calls @|untyped-fn|
with two strings instead of one string and one object.

@figure[
  @; TODO do more with this figure ... clearer
  "fig:ds-example"
  @elem{Untyped library, typed interface, and untyped client}
  fig:ds-example-y]

The question raised by this example is whether static types can catch the mistake
in the untyped client.
@|sDeep| and @|sshallow| types give opposite answers:
@itemlist[
@item{
  @|sDeep| types enforce the typed interface with run-time obligations for both
  the client and the library.
  Because the client sends a string where the type expects a font object,
  the client triggers a run-time type error.
}
@item{
  @|sShallow| types guarantee the local integrity of typed code, but nothing more.
  The untyped client is allowed to send any input to the untyped @|untyped-fn|
  function, including two strings, without causing a type-level error.
}]

@; TODO be clearer about strong vs weak soundness? May help with understanding
From a theoretical perspective, @|sshallow| types satisfy a type soundness
property and nothing more@~cite{vss-popl-2017,gf-icfp-2018}.
Soundness states that the type of an expression predicts the kinds of values
that evaluation can produce.
In typed code, these predictions are often specific and useful.
For example, an expression with a function type cannot evaluate to a number.
In untyped code, these predictions are trivial;
 soundness merely ensures a well-formed result.
A property that distinguishes @|sdeep| types from @|sshallow| is
complete monitoring@~cite{dtf-esop-2012,gfd-oopsla-2019}.
Semantics that satisfy complete monitoring enforce types as invariants
that all clients, typed or untyped, can rely on.



@figure[
  "fig:model:base-interaction"
  @elem{Outline for @|sdeep|, @|sshallow|, and @|suntyped| interactions}
  @; fig:model-interaction-y
  @exact|{ \newcommand{\bigicon}[1]{\scalebox{1.6}{\(#1\)}}\begin{tikzpicture}[tips=proper]

  \node (S) {\bigicon{\sS}};
  \node (D) [right of=S,xshift=1.4cm,yshift=8mm] {\bigicon{\sD}};
  \node (U) [right of=D,xshift=1.4cm,yshift=-8mm] {\bigicon{\sU}};

  \draw [>=latex,<->,very thick] (S) edge [bend left=7] node [above,xshift=-1mm] {\(\swrap\)} (D);
  %\draw [>=latex,<->] (D) edge [bend left=7] (S);

  \draw [>=latex,<->,very thick] (D) edge [bend left=7] node [above,xshift=2mm] {\(\swrap\)} (U);
  %\draw [>=latex,->] (U) edge [bend left=7] (D);

  \draw [>=latex,->,very thick] ([yshift=-0.5mm] S.east) edge [bend right=7] node [above,yshift=-1mm] {\(\snoop\)} ([yshift=-0.5mm] U.west);
  \draw [>=latex,->,very thick] ([yshift=-2mm] U.west) edge [bend left=7] node [below] {\(\sscan\)} ([yshift=-2mm] S.east);

  %\draw [->] (S) edge [loop, min distance=2mm,in=240,out=200,looseness=3] node [below] {\(\snoop\)} (S);
  %\draw [->] (D) edge [loop, min distance=2mm,in=110,out=70,looseness=3] node [above] {\(\snoop\)} (D);
  %\draw [->] (U) edge [loop, min distance=2mm,in=300,out=340,looseness=3] node [below] {\(\snoop\)} (U);

\end{tikzpicture}\vspace{-2ex} }|]

@subsection{@|sNatural| Semantics}

One way to implement @|sdeep| types is the @|sNatural|
semantics@~cite{tf-dls-2006,mf-toplas-2009,st-sfp-2006}.@note{@|sNatural| is a.k.a.
Guarded@~cite{vksb-dls-2014}, Behavioral@~cite{clzv-ecoop-2018}, and
Deep@~cite{tgpk-dls-2018}.}
Natural interprets types as contracts in a straightforward manner.@note{
Researchers are actively seeking improved variants of @|sNatural|@~cite{htf-hosc-2010,g-popl-2015,stw-pldi-2015,gct-popl-2016}
and measuring the efficiency of implementations@~cite{fgsfs-oopsla-2018,kas-pldi-2019}.
Theoretical results about @|sNatural| hold for these semantics-preserving variants as well.
}
For example, base types are enforced with predicate checks,
types for immutable values are enforced with first-order traversals,
and types for higher-order values such as arrays and functions are enforced with
higher-order @emph{wrapper} contracts.
Because each contract fully enforces a type, these contracts need only guard
the boundaries between typed and untyped code.
Within typed modules, code can run efficiently and employ type-directed
optimizations@~cite{tscff-pldi-2011}.


@subsection{@|sTransient| Semantics}

The @|sTransient| semantics is an implementation of @|sshallow| types that
does not require wrappers@~cite{vss-popl-2017}.
@|sTransient| enforces types by injecting first-order
checks throughout typed pieces of code:
typed, public functions must check their inputs;
typed modules must check their untyped imports; and
typed expressions must check the results computed during a function call,
the elements extracted from a data structure, and the outcome of any
downcasts.
In @figure-ref{fig:model:base-interaction}, these conditions imply one check:
the typed interface must check that @|untyped-fn| is a function.
In general, every line of typed code may add several
@|sTransient| checks, but each check is inexpensive.
By contrast to higher-order contracts, the checks
do not traverse values and do not impose allocation and indirection
costs.


@;@section{Why @|sTransient| and @|sNatural|?}
@; @; 2021-06-30 : the intro already covers this
@;
@;@|sNatural| and @|sTransient| are prime candidates for a three-way combination
@;because they explore distinct points in a complex design space.
@;@|sNatural| depends critically on higher-order contracts, and can impose a
@;high run-time cost if the boundaries in a codebase call for frequent and
@;layered contract checks@~cite{tfgnvf-popl-2016,gtnffvf-jfp-2019}.
@;@|sTransient| takes an opposite, first-order approach and spreads checks
@;throughout typed code.
@;Having the ability to switch between @|snatural| and @|stransient| might address
@;the shortcomings of each.
@;Additionally, if these semantics can interoperate then other less-extreme
@;combinations should be well in reach.

