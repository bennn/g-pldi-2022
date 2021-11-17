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
Switching between the two is a one-line change.

For the most part, the model was an effective guide for the implementation.
@|sDeep| and @|sShallow| Racket share a common surface syntax, type checker,
and evaluation syntax.
The question was how to modify these compiler back-ends to produce code
with context-dependent runtime checks.
Unexpected challenges arose regarding separate compilation and
the enforcement of @|sdeep| types with wrapping higher-order contracts.
Metaprogramming also raised issues, but these are deferred to an @appendixref{appendix:macro}
to keep the paper widely-applicable.


@section[#:tag "sec:implementation:ctc-indirect"]{Wrapping Contracts and Type Environments}

Higher-order exports from @|sdeep|-typed code need protection from @|suntyped|
and @|sshallow|-typed clients.
Wrapping contracts are a convenient way to implement this
protection because they let @|sdeep| modules share exports with no performance
overhead.
@; A @|sdeep| client may use an unwrapped identifier as long as @|suntyped| and
@; @|sshallow| clients receive a wrapped one.
They introduce a problem, however, because the type checker for @|sshallow|
code must associate a type with these wrappers to understand uses of
@|sdeep|-typed identifiers.

In Typed Racket, all exports from @|sdeep| code statically resolve to either an
unwrapped identifier or a wrapped one depending on the context in which they
are used@~cite{ctf-sfp-2007,tscff-pldi-2011}.
The wrappers do not have types due to the organization of
compiled code,@note{
Type environment information compiles to a submodule@~cite{f-gpce-2013}.
Wrappers compile to a ssecond submodule to delay the cost of
building them.}
but they do come with a compile-time pointer to the unwrapped identifier.
@|sShallow| Racket follows these pointers to typecheck interactions.



@section[#:tag "sec:implementation:ctc-gen"]{Where to Create @|sShallow|-to-@|sDeep| Contracts}
@; TODO debug diary, use optional to introduce
@; ... maybe another diary for prefabs

@|sDeep|-typed code needs to validate imports from @|suntyped| and
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


@section[#:tag "sec:implementation:api"]{Three-way Boundary Utilities}
@; TB api

Static types and higher-order contracts are fundamentally different
tools.
Types enable proofs via static analysis.
Contracts check behaviors dynamically.
For certain types, such as a type for terminating functions@~cite{ngtv-pldi-2019},
it is difficult to generate a precise contract.
A language may therefore wish to offer an API that lets programmers specify the
contracts that enforce @|sdeep| types at a boundary.
These APIs need to be generalized for a three-way implementation.

Typed Racket comes with two tools for manipulating type boundaries.
One expects an typed identifier and a subtype of the identifier's actual type;
it uses the subtype to generate a contract.
@; With @|sshallow| types in the mix, the restriction can lead to type errors.
The other tool combines two identifiers into a context-sensitive export.
To support @|sshallow| types, the first tool must create a (sub-)typed and
wrapped export and the second tool must accept a third identifier.
Refer to the @appendixref{appendix:boundary-api} for additional details.

