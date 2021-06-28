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

@Figure-ref{fig:ds-example} presents a three-module program to illustrate
the different policies.
The untyped module on the left contains a stub definition for a function
@tt{text-icon} that accepts two arguments.
This module is a greatly simplified picture of Racket's @tt{images/icons/symbol}
module, which depends on a few thousand lines of rendering and raytracing code.
The typed module in the middle is an interface for the untyped function;
The types correctly state that @tt{text-icon} expects a string and a font
object and computes a bitmap object.
Finally, the untyped module on the right attempts to call @tt{text-icon}
with bad input; specifically, a string and a symbol.

@figure*[
  "fig:ds-example"
  @elem{Untyped library, typed interface, and untyped client}
  fig:ds-example]

The question raised by this example is whether static types can catch mistakes
in untyped code.
The literature presents two answers, which we characterize as @emph{@|sdeep|}
and @emph{@|sshallow|} types:
@itemlist[
@item{
  @|sDeep| types enforce the interface with run-time obligations for both
  the client and the library.
  In this case, the client triggers a run-time error because the symbol
  @tt{'modern} is not a font object.
}
@item{
  @|sShallow| types ensure the local integrity of typedd code, but nothing more.
  Untyped clients may send any input to the untyped @tt{text-icon} function;
  the result of running @figure-ref{fig:ds-example} depends on how the library
  function reacts to the symbol.
}]

Formally, @|sdeep| and @|sshallow| types are characterized by type soundness
and complete monitoring theorems.




TODO
- sec model
  + @|sDeep| types are characterized by complete monitoring


@section{@|sNatural| Semantics}

impl strtegy, boundaries?, pros and cons


@section{@|sTransient| Semantics}

mirror natural, yes yes



@section{Why @|sTransient| and @|sNatural|?}

complementary, perhaps!
no other strategies so extreme
if nothing else baseline for future experiments

@; Natural and Transient are only realistic options.
@; Right@~cite{gdf-draft-2020}.

ack pycket etc.?


