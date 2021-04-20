#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS Deep + Shallow is better than Deep or Shallow
@;  - saves guarantees, performance, and expressiveness
@;  - TODO is expressiveness incidental?
@;  - maybe add constraints = no new compiler, no new language

@; 2021-04-06 the "disappointing success" pitch doesn't work, it's too focused on performance
@;  can't mix 2 slow semantics to get faster than erasure. No way!

@; ---

@; - gradual typing ... tradeoff guarantees expr perf ...
@; - incompatible designs ... some cannot coexist ... others can lets try
@; - competing strengths
@; - sound familiar?
@;   - gt = static issues
@;   - now, runtime issues
@;   - ps like types
@; - obvious starting point = Nat + Trans
@; - contributions = theory + impl + benefits guarantees expressiveness perf
@; ... question is benefits and synergy ... more to do later


@title[#:tag "sec:introduction"]{A Spectrum of Type Enforcement}

Gradual typing research presents several @emph{type-enforcement strategies}
for combining typed code with untyped code@~cite[gf-icfp-2018].
Each strategy uses run-time assertions to guarantee that untyped code cannot
undermine certain parts of the static types.
For example, the @emph{Natural} strategy enforces full types with higher-order contracts,
the @emph{Transient} strategy enforces type constructors with first-order tests,
and the @emph{Concrete} strategy enforces full types by tagging newly-created values
and imposing tag checks.
@; TODO cite all of the above
Further variations abound.

Unsurprisingly, the various strategies in the literature have benefits and
drawbacks.
Two extremes are the Natural and Transient strategies.
Natural offers strong guarantees, in particular type soundness and complete
monitoring@~cite[gfd-oopsla-2019], but may require a huge performance cost
and depends on wrappers that can be overly conservative.
Transient offers weak guarantees, but uses simpler checks that are
easier to implement and have more promising costs.

The question thus arises, whether one language can incorporate two
type-enforcement strategies in addition to untyped code.
Such a language presents three dialects to programmers:
two typed variants and one untyped variant.
For these dialects to be meaningful, each must satisfy the same properties in
the mix that it did in the original.
For the mix to be useful, the parts must have competing advantages.



This paper supports two general claims:
(1) Deep and Shallow types can safely interoperate,
and (2) combining both into one language offers measurable benefits.
The evidence for these claims has three parts.

@itemlist[
@item{
  A model language with proofs of type soundness and complete monitoring (@secref{sec:model}).
  The language supports Deep-typed code, Shallow-typed code, and untyped code.
  The proofs establish the integrity of the different types.
}
@item{
  An implementation that combines standard Deep Typed Racket@~cite[tfffgksst-snapl-2017] with a Shallow-typed variant@~cite[todo] (@secref{sec:implementation}).
  The Deep and Shallow halves of the implementation stand on equal footing;
   every Deep-typed program can be expressed in Shallow Racket.
}
@item{
  A rigorous evaluation of the artifact that covers three bases: performance, guarantees, and expressiveness (@secref{sec:evaluation}).
  The performance evaluation uses the GTP benchmark suite.
  @; TODO findings?
  Case studies and mailing list archives illustrate the other benefits.
}
]



