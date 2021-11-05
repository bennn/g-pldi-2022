#lang scribble/acmart @acmsmall @10pt @screen
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
implementation of @|stransient|@~cite{glfd-pj-2021}.
Programmers can access these options via module
languages: @tt{typed/racket} for @|sdeep| types and
@tt{typed/racket/shallow} for @|sshallow|.
Switching between options is a one-line change.

For the most part, the model was an effective guide for the implementation.
Unexpected challenges arose regarding separate compilation and
the enforcement of @|sdeep| types with wrapping higher-order contracts.
Metaprogramming also raised issues, but these are deferred to an appendix
to keep the paper widely-applicable.


@section[#:tag "sec:implementation:ctc-indirect"]{Type Lookup and Wrapping Contracts}

Higher-order exports from @|sdeep|-typed code need protection from @|suntyped|
and @|sshallow|-typed clients.
Wrapping contracts at the boundaries are a convenient way to implement this
protection because they let @|sdeep| modules share exports with no performance
overhead.
@; A @|sdeep| client may use an unwrapped identifier as long as @|suntyped| and
@; @|sshallow| clients receive a wrapped one.
Type lookup in @|sshallow| code, however, must be aware of these wrappers
to understand uses of @|sdeep|-typed identifiers.

In Typed Racket, all exports from @|sdeep| code get compiled to
@emph{rename transformers} that statically resolve to either an
unwrapped identifier or a wrapped one@~cite{ctf-sfp-2007,tscff-pldi-2011}.
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
This approach lets multiple clients reference one set of contract definitions.


@section[#:tag "sec:implementation:api"]{Three-way Boundary Utilities}
@; TB api

Static types and higher-order contracts are fundamentally different
tools.
Types enable proofs via static analysis.
Contracts check behaviors dynamically.
For certain types, such as a type for terminating functions@~cite{ngtv-pldi-2019},
it is difficult to generate a precise contract.
A language may therefore wish to offer an API that lets programmers
underapproximate the contracts that enforce @|sdeep| types at a boundary.
These APIs need to be generalized for a three-way implementation.

Typed Racket comes with two tools for manipulating type boundaries.
One expects an identifier and a subtype; it uses the subtype to
generate a (conservative, computable) contract.
@; With @|sshallow| types in the mix, the restriction can lead to type errors.
The other tool simply joins two identifiers into a context-sensitive export.
To support @|sshallow| types, both tools can use a third input.
Refer to the appendix for examples and more details.

