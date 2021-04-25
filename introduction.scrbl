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
for combining typed code with untyped code@~cite{gf-icfp-2018}.
Each strategy uses run-time assertions to guarantee that untyped code cannot
undermine certain parts of the static types.
For example, the @emph{Natural} strategy enforces full types with higher-order contracts@~cite{mf-toplas-2009,tf-dls-2006,st-sfp-2006},
the @emph{Transient} strategy enforces type constructors with first-order tests@~cite{vss-popl-2017},
and the @emph{Concrete} strategy enforces full types by tagging newly-created values
and imposing tag checks@~cite{wnlov-popl-2010,mt-oopsla-2017}.
Further variations abound.

Unsurprisingly, the various strategies in the literature have benefits and
drawbacks.
Two illustrative points of comparison are the Natural and Transient strategies.
Natural offers strong guarantees, in particular type soundness and complete
monitoring@~cite{gfd-oopsla-2019}, but may require a huge performance cost
and depends on wrappers that can be overly conservative.
Transient offers weak guarantees, but uses simpler checks that are
easier to implement and suggest a more promising cost model@~cite{vss-popl-2017,gm-pepm-2018,rmhn-ecoop-2019}.

The question thus arises, whether one language can incorporate two
type-enforcement strategies in addition to untyped code.
Such a multi-language offers three alternatives to programmers:
untyped code,
typed code with the first strategy, and
typed code with the second strategy.
For the mix to be meaningful, each part satisfy the same properties that it
did in the original.
For the mix to be useful, the parts must have complementary strengths.

@; TODO from Natural,Transient to Deep,Shallow ?

This paper supports two general claims:
(1) Deep and Shallow types can safely interoperate,
and (2) combining both into one language offers measurable benefits.
The evidence for these claims has three parts.

@itemlist[
@item{
  A model language with proofs of type soundness and complete monitoring (@secref{sec:model}).
  The language supports Deep-typed code, Shallow-typed code, and untyped code.
  The proofs establish the integrity of the different-strength types.
}
@item{
  An implementation that combines standard Deep Typed Racket@~cite{tfffgksst-snapl-2017} with a Shallow-typed variant@~cite{glfd-draft-2021} (@secref{sec:implementation}).
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



