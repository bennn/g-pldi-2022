#lang scribble/acmart
@(require "main.rkt" "bib.rkt"
   (only-in scriblib/footnote note)
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
@;
@; OTHER CHALLENGES
@; - submodule with default lang, need a way to get the "mode"
@; - forms like `cast` are really functions, need to come in N flavors, and can be mixed via require

@title[#:tag "sec:implementation"]{Implementation Challenges}

We have implemented three-way interactions atop Typed Racket.
The extension combines the standard ``@|sDeep|'' Typed Racket, which implements the
@|snatural| semantics@~cite{tf-popl-2008}, with the ``@|sShallow| Racket''
implementation of @|stransient|@~cite{gldf-pj-2021}.
Programmers may choose @|sdeep| or @|sshallow| types
when declaring a module.
@; these options via module
@; languages: @tt{typed/racket} for @|sdeep| types and
@; @tt{typed/racket/shallow} for @|sshallow|.
Switching between the two is a one-line change except in programs that
fine-tune the checks that guard type boundaries
(@section-ref{sec:implementation:api}).

For the most part, the model was an effective guide for the implementation.
@|sDeep| and @|sShallow| share a common surface syntax, type checker,
and evaluation syntax.
The key issue was how to modify these compiler back-ends to produce code
with context-dependent runtime checks.
Unexpected challenges arose regarding separate compilation,
the enforcement of @|sdeep| types, and metaprogramming.


@section[#:tag "sec:implementation:ctc-indirect"]{Wrapping Contracts and Type Environments}

Higher-order exports from @|sdeep|-typed code need protection from @|suntyped|
and @|sshallow|-typed clients.
Wrapping contracts are a convenient way to implement this
protection because they let @|sdeep| modules share exports with no performance
overhead.
@; A @|sdeep| client may use an unwrapped identifier as long as @|suntyped| and
@; @|sshallow| clients receive a wrapped one.
They introduce a problem with separate compilation, however, because the type checker for @|sshallow|
code must find a type for these wrappers to understand uses of
@|sdeep|-typed identifiers.

In Typed Racket, all exports from @|sdeep| code statically resolve to either an
unwrapped identifier or a wrapped one depending on the context in which they
are used@~cite{ctf-sfp-2007,tscff-pldi-2011}.
The wrappers do not have types due to the organization of compiled code.
Types appear in one submodule@~cite{f-gpce-2013} while wrappers appear
in a sibling submodule to delay the cost of building them.
But because the wrappers are implemented as Racket contracts@~cite{ff-icfp-2002},
they come with a compile-time pointer to the unwrapped identifier.
@|sShallow| Racket follows these pointers to typecheck interactions.


@section[#:tag "sec:implementation:ctc-gen"]{@|sShallow|-to-@|sDeep| Contracts}
@; TODO debug diary, use optional to introduce
@; ... maybe another diary for prefabs

@|sDeep|-typed code needs to wrap imports from @|suntyped| and
@|sshallow|-typed modules.
Because @|suntyped| imports lack types, the straightforward solution
is to ask programmers for a type-annotated import statement and
to generate contracts at the import.
@|sShallow| imports already have types.
This raises a question about where to prepare the validating contracts:
the exporting @|sshallow| module or the importing @|sdeep| module.

@|sShallow| Racket eagerly prepares contracts for its @|sdeep|-typed clients
and stores these contracts in a lazily-loaded submodule.
@; The process is the mirror-image of how a @|sDeep| Racket module protects
@; itself from @|sshallow| clients, which simplifies the implementation.
@; More substantially, ....
The main benefit of this approach is that multiple clients can reference one
set of contract definitions.


@section[#:tag "sec:implementation:macro"]{Macros and Hidden Exports}

Macro expansion may cause private identifiers from one
module to appear in the expansion of another module@~cite{f-popl-2016,fcdf-jfp-2012}.
If one module uses @|sdeep| types and the other uses @|sshallow|,
this behavior is a threat to type soundness.
The stowed identifiers must be protected like any other export.

By default, @|sDeep| and @|sShallow| Racket cannot share macros.
Programmers can enable reuse by exporting a macro unsafely.
An open question is whether a static analysis can determine
which macros may safely cross type boundaries.

@; example: rackunit macros
@; future: replace manual with a static analysis
@; future: protect stowed exports



@section[#:tag "sec:implementation:api"]{Three-way Boundary Utilities}
@; TB api

Static types and higher-order contracts are fundamentally different
tools.
Types enable proofs via static analysis.
Contracts check behaviors dynamically.
For certain types, such as a type for terminating functions@~cite{ngtv-pldi-2019},
it is difficult to generate an approximating contract.
A language may therefore wish to offer an API that lets programmers specify the
contracts that enforce @|sdeep| types at a boundary.
These APIs must be adapted to support a three-way implementation.

Typed Racket comes with two tools for type boundaries.
The first, @tt{require/untyped-contract}, expects a typed identifier and a subtype
of the identifier's actual type; it uses the subtype to generate a contract.
This behavior can make it somewhat harder to switch from @|sDeep| to @|sShallow| types.
For example, the standard array library
@; @render-lib[(make-lib "array library" "https://docs.racket-lang.org/math/array.html")]
uses this tool to give untyped code access to an overloaded function that
expects either an array of integers or an array of natural numbers.
Rather than generate a contract based on the overloaded type, which would
require a higher-order union contract, the library uses a subtype that
expects arrays of integers.
@|sShallow| code can access this array function as well, but only through the contract.
Switching a module from @|sDeep| to @|sShallow| may therefore require
casts to meet the subtype.

@;@bm{jpeg} benchmark (@section-ref{sec:evaluation:performance})
@;depends on @render-lib[(make-lib "math/array" "https://docs.racket-lang.org/math/array.html")])
@;@exact{\smallskip}
@;@typed-codeblock['(
@;  "(: check-array-shape"
@;  "   (-> (U (Vectorof Natural) (Vectorof Integer))"
@;  "       (Vectorof Natural)))")]
@;@exact{\smallskip}
@;@untyped-codeblock['(
@;  "(require/untyped-contract"
@;  "  [check-array-shape"
@;  "   (-> (Vectorof Integer) (Vectorof Natural))])")]

The second tool combines two identifiers.
In the following example, @tt{f} is defined as a context-sensitive identifier 
that expands to @tt{tf} in @|sDeep| code and to @tt{uf} in untyped code:
@tt{(define-typed/untyped-identifier f tf uf)}.
@;
@; @exact{\smallskip}
@; @;@typed-codeblock['(
@; @;  "(define-typed/untyped-identifier f tf uf)")]
@; @exact{\smallskip}
@;
@|noindent|@|sShallow| cannot be trusted with @tt{tf}
because of its weak soundness guarantee, and it cannot use
@tt{uf} if that identifier lacks a type.
Thus, the tool needs a third input for @|sShallow|
contexts.


