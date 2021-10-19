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

We have implemented three-way interactions as an extension to Typed Racket.
The extension combines standard ``@|sDeep|'' Typed Racket, which implements the
@|snatural| semantics@~cite{tf-popl-2008}, with the ``@|sShallow| Racket''
implementation of @|stransient|@~cite{glfd-pj-2021}.
Programmers can access these options via two module
languages: @tt{#lang typed/racket} for @|sdeep| types and
@tt{typed/racket/shallow} for @|sshallow|.
Switching between languages is a one-line change except in programs that
manipulate type boundaries directly.

For the most part, the model was an effective guide for the implementation.
Unexpected challenges arose regarding macros, separate compilation, and
the enforcement of @|sdeep| types with wrapping higher-order contracts.
@; The following sections describe the issues, both in general terms and
@; in the context of Typed Racket.


@section[#:tag "sec:implementation:ctc-indirect"]{Wrapping Contracts and Type Lookup}

Higher-order exports from @|sdeep|-typed code need protection from @|suntyped|
and @|sshallow|-typed clients.
Wrapping contracts are a compelling way to implement this protection because
they let @|sdeep| modules share exports with no performance overhead.
@; A @|sdeep| client may use an unwrapped identifier as long as @|suntyped| and
@; @|sshallow| clients receive a wrapped one.
Type lookup in @|sshallow| code, however, must be aware of these wrappers
to reason about its uses of @|sdeep|-typed identifiers.

In Typed Racket, all exports from @|sdeep| code get compiled to
@emph{rename transformers} that statically resolve to either an
unwrapped identifier or a wrapped one@~cite{tscff-pldi-2011}.
@; ctf-sfp-2007
The wrappers do not have types due to the organization of
compiled code,@note{
Type environment information gets compiled to a submodule@~cite{f-gpce-2013}.
Wrappers compile to a sibling submodule to delay the cost of
building them.}
but they do come with a compile-time pointer to the unwrapped identifier.
@|sShallow| Racket follows these pointers to typecheck
@|sdeep|/@|sshallow| interactions.



@section[#:tag "sec:implementation:ctc-gen"]{Annotation Burden and Contract Generation}
@; TODO debug diary, use optional to introduce
@; ... maybe another diary for prefabs

@|sDeep|-typed code needs to validate imports from @|suntyped| and
@|sshallow|-typed modules.
Because @|suntyped| imports also need types, their validation process is
straightforward.
Programmers must supply type annotations; the language can use these to
generate contracts.
@|sShallow| imports already have types.
This raises a question about when to prepare the validating contracts:
in the exporting @|sshallow| module or in the importing @|sdeep|
module.

@|sShallow| Racket eagerly prepares contracts for its @|sdeep|-typed clients
and stores these contracts in a lazily-loaded submodule.
The process is the mirror-image of how a @|sDeep| Racket module protects
itself from @|sshallow|-typed clients, which simplifies the implementation.
More substantially, this approach makes only one copy of the validating
contracts.


@section[#:tag "sec:implementation:macro"]{Macros and Hidden Exports}

Macro expansion may cause private identifiers from one
module to appear in (the expansion of) another module@~cite{f-popl-2016}.
@; fcdf-jfp-2012
If one module uses @|sdeep|-typed and the other uses @|sshallow|,
this behavior is a threat to type soundness.
The stowed identifiers must be protected/validated like any other export.

By default, Typed Racket prevents @|sDeep| Racket and @|sShallow| Racket
modules from sharing macros.
A programmer must inspect each macro and provide it unsafely
to enable reuse.
@; example: rackunit macros
@; future: replace manual with a static analysis
@; future: protect stowed exports


@section[#:tag "sec:implementation:api"]{Type-Boundary Utilities}
@; TB api

Static types and higher-order contracts are fundamentally different
tools.
Types enable proofs via static analysis.
Contracts check behaviors dynamically.
For certain types, such as a type for terminating functions@~cite{ngtv-pldi-2019},
it is hard to formulate an equivalent contract.
A language may therefore wish to offer an API that lets programmers
fine-tune the contracts that enforce @|sdeep| types at a boundary.
These APIs may cause friction in a three-way implementation.

Typed Racket comes with two tools for manipulating type boundaries.
One expects an identifier and a subtype; it uses the subtype to
generate a (conservative, computable) contract.
With @|sshallow| types in the mix, the restriction can lead to type errors.
The other tool simply joins two identifiers into a context-sensitive export.
To support @|sshallow| types, this tool must accept a third input.
Refer to the appendix for examples and more details.

