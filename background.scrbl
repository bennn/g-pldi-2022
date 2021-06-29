#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in scriblib/footnote note)
   (only-in "pict.rkt"
     fig:ds-example))

@; THESIS Natural and Transient are the only realistic options
@;  - more imagined ideas in JFP submission
@;  - side benefit, explore two extremes (wrappers / none)

@; - don't justify TR, we are simply using it
@; - keep this short, get to the model asap

@title[#:tag "sec:background"]{Background}

@section{Gradual Typing, Migratory Typing, Mixed-Typed Code}

Since 2006, language researchers have been investigating methods to combine
static and dynamic typing@~cite{st-sfp-2006,tf-dls-2006,mf-toplas-2009,gktff-sfp-2006}.
The ultimate goal is a @emph{mixed-typed language} that supports two kinds of
code in a convenient manner.@note{Following @citet{svcb-snapl-2015}, a @emph{gradual language}
is not a general mixed-typed language but rather a refinement that includes
a dynamic type which satisfies the graduality properties.}
Untyped code is free to perform any computation that the language can express.
Typed code is restricted to a subset of well-typed computations, but comes
with a guarantee that static types are meaningful predictions about run-time
behaviors.
Debates arise over what makes for a convenient mix.
Proponents of gradual typing call for a universal @tt{Dynamic} type that
helps to blur the distinction between typed and untyped code@~cite{svcb-snapl-2015}.
Proponents of migratory typing argue that a mixed-typed research should focus
on adding idiomatic types to existing languages@~cite{tfffgksst-snapl-2017} 
Other researchers have invented new mixed-typed languages that enforce types
in novel ways@~cite{wnlov-popl-2010,mt-oopsla-2017}.

The goal of this paper is to advance the state of mixed-typed languages in
general.
Consequently, the formal development adheres to two ground rules:
@itemlist[#:style 'ordered
@item{
  Use conventional, ahead-of-time techniques to enforce types at run-time.
}
@item{
  Focus on the interactions between standard types and untyped code.
}
]
@|noindent|These rules ensure a common baseline.
Researchers that are building new languages may be able to improve run-time
performance with advanced mechanisms for checking types.
Researchers studying gradual languages may wish to extend the theory with a
dynamic type.

@; conjecture: easy to add dyn


@section{@|sDeep| and @|sShallow| Types}

Within the design space of sound mixed-typed languages, there is surprising
disagreement about how types should guide the behavior of a program.
@; disagreement among whom?!
These disagreements are important because even small-looking decisions
can have far-reaching consequences when typed and untyped code interact.

@(define untyped-fn @tt{text})

@Figure-ref{fig:ds-example} presents a three-module program to illustrate
the different policies.
The untyped module on the left contains a stub definition for a function
@|untyped-fn| that accepts two arguments.
This module is a greatly simplified picture of Racket's @tt{images/icons/symbol}
module, which depends on a few thousand lines of rendering and raytracing code.
The typed module in the middle is an interface for the untyped function;
The types correctly state that @|untyped-fn| expects a string and a font
object and computes a bitmap object.
Finally, the untyped module on the right attempts to call @|untyped-fn|
with bad input; specifically, a string and a symbol.

@figure*[
  "fig:ds-example"
  @elem{Untyped library, typed interface, and untyped client}
  fig:ds-example]

The question raised by this example is whether static types can catch mistakes
in untyped code.
The literature presents two extreme answers, @emph{@|sdeep|} and @emph{@|sshallow|} types:
@itemlist[
@item{
  @|sDeep| types enforce the interface with run-time obligations for both
  the client and the library.
  In this case, the client triggers a run-time error because the type expects
  a font object.
}
@item{
  @|sShallow| types ensure the local integrity of typed code, but nothing more.
  Untyped clients may send any input to the untyped @|untyped-fn| function;
  the result of running @figure-ref{fig:ds-example} depends on how the library
  function reacts to the symbol.
}]

@; ... one way to formalize
From a theoretical perspective, @|sshallow| types satisfy a type soundness
property and nothing more@~cite{vss-popl-2017,gf-icfp-2018}.
Soundness states that the type of an expression predicts the kinds of values
that evaluation can produce.
In typed code, these predictions are often useful; for example, an expression
with a function type cannot evaluate to a number.
In untyped code, however, soundness merely ensures a well-formed result---hence
an untyped client may soundly invoke an untyped function with any sort of argument.
The property that distinguishes @|sdeep| types from @|sshallow| is
complete monitoring@~cite{dtf-esop-2012,gfd-oopsla-2019}.
A semantics that satisfies complete monitoring is able to enforce type obligations
on every interaction, thus giving an interface the ability to constrain behaviors.
@; every interaction that WHAT?


@section{@|sNatural| Semantics}

One way to implement @|sdeep| types is the @emph{natural} (aka. guarded)
semantics@~cite{tf-dls-2006,mf-toplas-2009,st-sfp-2006} 
Natural interprets types as contracts in a straightforward manner.@note{
Researchers are actively seeking improved variants of natural@~cite{htf-hosc-2010,g-popl-2015,stw-pldi-2015,gct-popl-2016}
and measuring the efficiency of implementations@~cite{fgsfs-oopsla-2018,kas-pldi-2019}.
Theoretical results about natural automatically apply to these semantics-preserving variants.
}
For example, base types are enforced by predicate checks,
types for immutable data are enforced by first-order traversals,
and types for higher-order data such as arrays and functions are enforced by
higher-order contracts.
Because each contract fully enforces a type, these contracts need only guard
the boundaries between typed and untyped code.
Within typed modules, code can run efficiently and employ the same type-directed
optimizations as any non-mixed typed language.


@section{@|sTransient| Semantics}

The @|stransient| semantics is a compelling way to implement @|sshallow| types@~cite{vss-popl-2017}.
@; compelling?!
Transient protects typed code with simple first-order checks that validate
the top-level shape of values.
These checks guard the boundaries between typed and untyped code, but also
appear throughout typed modules.
Typed, public functions must check their inputs 
Typed expressions must check the results computed during a function call,
the elements extracted from a data structure, and the outcome of any type-level
downcasts.


@section{Why @|sTransient| and @|sNatural|?}

@|sNatural| and @|sTransient| are prime candidates for a three-way combination
because they explore distinct points in a complex design space.
@|sNatural| depends critically on higher-order contracts, and can impose a
high run-time cost if the boundaries in a codebase call for frequent and
layered contract checks@~cite{tfgnvf-popl-2016,gtnffvf-jfp-2019}.
@|sTransient| takes an opposite, first-order approach and spreads checks
throughout typed code.
Having the ability to switch between @|snatural| and @|stransient| might address
the shortcomings of each.
Additionally, if these semantics can interoperate then other less-extreme
combinations should be well in reach.


