#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in "pict.rkt"
     fig:any-wrap
     fig:no-wrap
     fig:index-of
     untyped-codeblock
     typed-codeblock))

@; THESIS the TR compiler can support shallow
@; - outside-facing API : new lang for users, easy switch
@; - inside-facing API : reuse ~80% of compiler
@; - note typed libraries still deep
@; - note awkardness at typed-untyped utils
@;
@; - for technical details, point to PR or dissertation

@title[#:tag "sec:implementation"]{IMPLEMENTATION}
@; Theory to Practice: Typed Racket}
@; 2021-04-29 do we really need to discuss typed/untyped API? could point to dissertation for details

According to the model, a mixed-typed language can support three-way
interactions by compiling types to contracts at @|sdeep|-typed boundaries
and to checks throughout @|sshallow|-typed code.
The model, however, is an abstraction that ignores practical issues.
To show that a realistic language can also support @|sdeep| and @|sshallow|
types, the authors have extended Typed Racket to combine its standard
@|snatural| semantics@~cite{tf-popl-2008} with a @|stransient| one@~cite{glfd-pj-2021}.
The two semantics coexist as modes of one compiler codebase and rely on the
same static type checker.
Unexpected challenges arose regarding the use of type definitions across
@|sdeep| and @|sshallow| modules and the adaptation of an API that customizes
@|sdeep|--@|suntyped| boundaries.


@section[#:tag "sec:implementation:overview"]{User-Facing API}

All Racket modules begin with a @tt{#lang} declaration.
The combined language introduces two concepts,
@|sDeep| Typed Racket and @|sShallow| Typed Racket,
and thus offers three basic module languages:
@itemlist[
@item{
  @tt{#lang typed/racket/deep} provides the @|sdeep| @|snatural| semantics, which
  enforces the types at module boundaries with higher-order contracts
  (same as the @tt{typed/racket} language).
}
@item{
  @tt{#lang typed/racket/shallow} provides a @|sshallow|, @|stransient| semantics that
  checks type shapes at all boundaries and all elimination forms.
}
@item{
  @tt{#lang racket} provides the untyped Racket semantics.
}
]

Both @|sdeep| and @|sshallow| modules rely on the same type checker, thus
programmers can easily switch between them.
If a module is well-typed in the @|sdeep| language, then with few exceptions
(@section-ref{sec:implementation:impl:tu}) it is well-typed in @|sshallow|.
Same goes for the reverse direction, when changing the @tt{#lang} line
from @|sshallow| to @|sdeep|.

@|sShallow| types are more permissive than @|sdeep| types at run-time.
Operationally, first-order checks enforce fewer properties than higher-order
contracts.
Thus, the set of programs that run to completion with @|sshallow| types is
a superset of the programs that run with @|sdeep| types.
@Section-ref{sec:evaluation:expressiveness} explores this semantic gap in greater detail.


@;@section[#:tag "sec:implementation:internals"]{Three-way Internals}
@;
@;The Racket compiler processes an untyped module in two steps.
@;First, the macro expander simplifies expressions within the module to
@;a kernel language@~cite{f-icfp-2002,f-popl-2016}.
@;Second, the compiler backend optimizes the kernel code and tranforms it to a
@;native representation@~cite{fddkmstz-icfp-2018}.
@;
@;@|sDeep| Racket injects three passes between the macro expander
@;and the compiler backend@~cite{tscff-pldi-2011}.
@;The first pass is the type checker.
@;The second pass generates contracts for module exports.
@;The final pass is a type-directed optimizer@~cite{stff-padl-2012} that generates
@;efficient untyped code for the compiler backend to further optimize.
@;
@;@|sShallow| Racket is implemented as a second mode of the Typed Racket compiler@~cite{g-thesis-2020,glfd-pj-2021}.
@;It too injects three passes between the macro expander and compiler backend,
@;but with changes appropriate to the @|stransient| semantics.
@;The first pass, the type checker, remains unchanged.
@;The second pass is entirely different.
@;Instead of generating contracts at boundaries, @|sShallow| Racket generates
@;first-order checks and additionally traverses the module to put a check around
@;every potentially-unsafe elimination form.
@;The final pass is an optimizer that performs a subset of the @|sDeep| Racket optimizations.
@;Every optimization that is sound for weak type soundness is in the subset.
@;Others, notably the dead-code elimination pass, stay out.
@;
@;The unexpected challenges for the three-way implementation arose in two
@;places: at the boundaries between @|sDeep| and @|sShallow| modules
@;and in Typed Racket's boundary API.
@;
@;
@;@subsection[#:tag "sec:implementation:impl:code"]{@|sDeep| and @|sShallow| Interaction}
@;
@;Racket supports both separate compilation and hygienic macros@~cite{f-icfp-2002}.
@;Each module in a program compiles to a core language and other modules can
@;re-use the output.
@;Typed Racket cooperates with the separate compilation protocol by serializing
@; the results of type checking@~cite{tscff-pldi-2011,ctf-sfp-2007}.
@;A well-typed module compiles to untyped code (with appropriate contracts)
@; and a local type environment.
@;When one @|sDeep| module imports from another, the complied environment enables
@;faster type checking.
@;
@;At first glance, it appears that @|sShallow| code can use the same protocol
@; to find the type of @|sDeep| imports.
@;Wrappers get in the way, unfortunately, because of how @|sDeep| Racket protects
@;its exports.
@;For every public identifier in a @|sDeep| module, the compiler generates
@;a public macro that can expand to either the typed identifier or an untyped wrapper.
@;@|sShallow| code must use the wrappers, thus it also needs a way to uncover
@;their static types.
@;A similar problem arises when @|sDeep| code imports an identifier from
@;@|sShallow| Racket.
@;
@;For @|sdeep|-to-@|sshallow| exports, the solution is to modify
@;type lookups to pass through wrappers.
@;Fortunately, the change is possible through the compile-time API for
@;Racket contracts.
@;The compiler associates each wrapped identifier with a structure that links
@;back to the original identifier.
@;The @|sShallow| type checker looks out for these wrappers and uncovers
@;the originals as needed.
@;
@;@|sShallow|-to-@|sDeep| exports use a dual method.
@;The type checker inspects wrapped values to see whether there is an
@;available type.
@;Such types allow static type checks to succeed, and at run-time the wrapper
@;keeps @|sDeep| code safe.
@;
@;Consequently, a @|sShallow| module must
@;be prepared to create wrappers for its exports.
@;The wrapper-making code is generated during compilation, at the end of
@;type checking, but it does not run until needed by a @|sDeep| client.
@;In this way, only programs that depend on @|sDeep| code suffer from the
@;expressiveness limits of wrappers.
@;
@;
