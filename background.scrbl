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

@; TODO focus on research, not people
For the past two decades, researchers have been investigating methods to combine
static and dynamic typing@~cite{st-sfp-2006,tf-dls-2006,mf-toplas-2009,gktff-sfp-2006}.
The ultimate goal of these efforts is a mixed-typed (or gradual@note{Following @citet{svcb-snapl-2015},
we prefer to reserve the name @emph{gradual} for mixed-typed languages with
a dynamic type that satisfies the gradual guarantees / graduality@~cite{nla-popl-2019}.})
language that supports two styles of code in a convenient manner.
Untyped code is free to perform any computation that the language can express.
Typed code is restricted to a subset of well-typed computations, but comes
with a guarantee that static types are meaningful predictions about run-time
behaviors.
Distinctions arise over what makes for a convenient mix.
Gradually-typed languages include a universal @tt{Dynamic} type that
helps to blur the distinction between typed and untyped code@~cite{svcb-snapl-2015}.
Migratory typing systems add
idiomatic types to an existing language@~cite{tfffgksst-snapl-2017} 
Other methods include the development of novel languages@~cite{wnlov-popl-2010,mt-oopsla-2017,kas-pldi-2019,rmhn-ecoop-2019}
and/or compilers@~cite{rat-oopsla-2017,bbst-oopsla-2017}.

The goal of this paper is to advance the state of mixed-typed languages in a
general setting.
Consequently, the formal development adheres to two ground rules:
@itemlist[#:style 'ordered
@item{
  Use ahead-of-time techniques to enforce types at run-time.
}
@item{
  Focus on the interactions between conventional types and untyped code.
}
]
@|noindent|These rules ensure a widely-applicable baseline.
Specializations are a promising subject for future work.
Advanced mechanisms for checking types may yield better performance@~cite{kas-pldi-2019,vsc-dls-2019,bbst-oopsla-2017,rmhn-ecoop-2019}.
Gradual languages can motivate the addition of a dynamic type.


@section{@|sDeep| and @|sShallow| Types}

@(define untyped-fn @tt{text})

Within the realm of sound mixed-typed languages, designs
do not agree on how types should guide the behavior of a program.
Two leading alternative for run-time properties are @|sdeep| and @|sshallow| types.
Roughly speaking, @|sdeep| types try to be equally strong as conventional
static types while @|sshallow| types strike a balance between guarantees and
pragmatic benefits.

@Figure-ref{fig:ds-example} presents a three-module program to illustrate
the gap between @|sdeep| and @|sshallow|.
The untyped module on the left contains a stub definition for a function
@|untyped-fn| that accepts two arguments.
This module is a greatly simplified picture of Racket's @tt{images/icons/symbol}
module, which incorporates thousands of lines of rendering and raytracing code.
The typed module in the middle is an interface for the untyped function.
The types correctly state that @|untyped-fn| expects a string and a font
object and computes a bitmap object.
Finally, the untyped client module on the right mistakenly calls @|untyped-fn|
with two strings instead of one string and one font object.

@figure*[
  "fig:ds-example"
  @elem{Untyped library, typed interface, and untyped client}
  fig:ds-example]

The question raised by this example is whether static types can catch the mistake
in the untyped client.
@|sDeep| and @|sshallow| types give opposite answers:
@itemlist[
@item{
  @|sDeep| types enforce the interface with run-time obligations for both
  the client and the library.
  In this case, the client triggers a run-time error because the type asks for
  a font object.
}
@item{
  @|sShallow| types ensure the local integrity of typed code, but nothing more.
  The untyped client is allowed to send any input to the untyped @|untyped-fn|
  function, including two strings.
}]

@; TODO be clearer about strong vs weak soundness? May help with understanding
From a theoretical perspective, @|sshallow| types satisfy a type soundness
property and nothing more@~cite{vss-popl-2017,gf-icfp-2018}.
Soundness states that the type of an expression predicts the kinds of values
that evaluation can produce.
In typed code, these predictions are often useful; for example, an expression
with a function type cannot evaluate to a number.
The catch is that untyped code makes trivial predictions; soundness merely
ensures a well-formed result.
The property that distinguishes @|sdeep| types from @|sshallow| is
complete monitoring@~cite{dtf-esop-2012,gfd-oopsla-2019}.
A semantics that satisfies complete monitoring is able to enforce type obligations
on every interaction, thus giving an interface the ability to constrain behaviors.


@section{@|sNatural| Semantics}

One way to implement @|sdeep| types is the @|sNatural| (aka. guarded)
semantics@~cite{tf-dls-2006,mf-toplas-2009,st-sfp-2006} 
Natural interprets types as contracts in a straightforward manner.@note{
Researchers are actively seeking improved variants of @|sNatural|@~cite{htf-hosc-2010,g-popl-2015,stw-pldi-2015,gct-popl-2016}
and measuring the efficiency of implementations@~cite{fgsfs-oopsla-2018,kas-pldi-2019}.
Theoretical results about @|sNatural| automatically apply to these semantics-preserving variants.
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

The @|sTransient| semantics is an implementation of @|sshallow| types that
does not require higher-order wrappers@~cite{vss-popl-2017}.
@|sTransient| protects typed code by rewriting it to include simple first-order
checks.
These checks guard the boundaries between typed and untyped code, but also
appear throughout typed modules.
Typed, public functions must check their inputs 
Typed expressions must check the results computed during a function call,
the elements extracted from a data structure, and the outcome of any type-level
downcasts.
This pay-as-you go strategy means that every new line of typed code may
impose a cost, but the unit cost of each check is typically low.


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


