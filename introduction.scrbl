#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt" (only-in scriblib/footnote note))

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


@; Some of these strategies place restrictions on untyped code.
@; For example, the Concrete strategy uses a tagging protocol that puts
@; untyped and typed values on unequal footing@~cite{wnlov-popl-2010,sfrbcsb-popl-2014,rzv-ecoop-2015,mt-oopsla-2017}.
@; Other strategies erase types and provide no run-time support@~cite{bat-ecoop-2014}.
@; The focus of this paper is on strategies that are compatible with an untyped
@; host language and provide a basic type soundness guarantee.

@title[#:tag "sec:introduction"]{A Spectrum of Type Enforcement}

Taken broadly, the research area of gradual typing presents several
@emph{type-enforcement strategies} that enforce static types against run-time
interactions with untyped code.
Among strategies that are compatible with an untyped
host language and provide a basic type soundness guarantee, two promising
alternatives are @|sNatural|@~cite{mf-toplas-2009,tf-dls-2006,st-sfp-2006}
and @|sTransient|@~cite{vss-popl-2017}.
The @|sNatural| strategy uses higher-order contracts to enforce the @|sdeep|
behavioral claims implied by higher-order types.
The @|sTransient| strategy uses first-order checks to enforce @|sshallow| aspects
of types in type-annotated code.
Unsurprisingly, these different methods of enforcing types come with
benefits and drawbacks.
Contracts in @|sNatural| enable strong type soundness
and complete monitoring guarantees@~cite{gfd-oopsla-2019},
but can impose a huge performance cost@~cite{gtnffvf-jfp-2019}.
First-order checks in @|sTransient| enable only a weak soundness guarantee,
but are far less likely to dominate the running time of a program@~cite{vss-popl-2017,gm-pepm-2018,rmhn-ecoop-2019}.

The question thus arises as to whether two enforcement strategies can
interoperate, and thereby give programmers the ability to use @|sdeep| types
when guarantees matter and @|sshallow| types to avoid performance bottlenecks.
This paper provides an affirmative answer: 
@|sdeep| and @|sshallow| types can interoperate without sacrificing
their formal properties.
Futhermore, the combination brings measurable benefits.
The evidence for these claims has three parts:

@itemlist[
@item{
  A model language with proofs of type soundness and complete monitoring (@secref{sec:model}).
  The language supports @|sdeep|-typed code, @|sshallow|-typed code, and untyped code
  via a semantics that applies ideas from @|sNatural| and @|sTransient|.
  The proofs establish the integrity of types.
}
@item{
  An implementation that combines the original @|sDeep| Typed Racket@~cite{tfffgksst-snapl-2017} with a @|sshallow|-typed variant@~cite{glfd-pj-2021} (@secref{sec:implementation}).
  The @|sdeep| and @|sshallow| halves of the implementation stand on equal footing.
  Indeed, every @|sdeep|-typed program can be expressed in @|sShallow| Racket.
}
@item{
  An evaluation of the guarantees, expressiveness, and performance
  of the Typed Racket implementation (@secref{sec:evaluation}).
  The performance study uses the GTP benchmark suite.@note{@|gtp-url|}
  The expressiveness findings are driven by questions posted to the Typed Racket
  mailing list.
}
]

In general, this paper is the first to combine type-sound gradual
typing strategies in a way that gives programmers control over the
protection/performance tradeoff in their code.
The combination is coarse and the two typed semantics cannot cooperate as
well as two wrapping semantics might (@section-ref{sec:future}), but
the evaluation suggests that interoperability is a promising way to address
tradeoffs without restricting the source language or replacing the compiler.


