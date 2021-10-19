#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt" (only-in "pict.rkt" fig:opt0 fig:opt1))

@title[#:tag "sec:future"]{Future Work}

One drawback apparent in the model is that @|sdeep| and @|sshallow| cannot
trust one another.
@|sDeep| code must wrap inputs from @|sshallow| code because they may have
originated in untyped code.
@citet{g-thesis-2020} sketches two ideas for removing checks from
@|sdeep|--@|sshallow| boundaries.
One requires an escape analysis and the other asks @|sshallow| code to
create wrappers.
A third approach is to adapt confined gradual typing@~cite{afgt-oopsla-2014}.
If the type system can prove that confined values originate in typed code and
never escape to untyped, then @|sdeep| and @|sshallow| can freely share these
values.
These are all potential directions for future work.

@; A second line of future work is to incorporate a dynamic type
@; that satisfies the graduality properties@~cite{svcb-snapl-2015}.
@; For the model, the authors conjecture that @|sdeep| and @|sshallow| soundness
@; can be achieved by adding a new wrapper for dynamic-typed values and nothing
@; more.
@; Implementing the dynamic type may pose new challenges, especially when it
@; comes to blame and if the implementation targets an existing
@; static language@~cite{g-snapl-2019}.
@; @; HO HUM ... cut this paragraph?

A second future direction is to identify best practices for coding in
a three-way language.
Anecdotal experience suggests the following strategy:
@itemlist[#:style 'ordered
@item{
  Start by adding @|sdeep| types because their strong guarantees may help
  identify logical errors.
}
@item{
  If performance becomes an issue, switch to @|sshallow|.
}
@item{
  Once all high-traffic boundaries
  are typed, switch back to @|sdeep| to maximize performance.
}
]
@|noindent|The challenge is to systematically test the effectiveness of this
migration story.
Meanwhile, there may be additional ways to leverage the spectrum of
type enforcement.




@; The reason is trat @|stransient| lacks a way of learning from past checks.
@; The model is safe, but makes @|sdeep| types expensive.
@; Every boundary to @|sdeep| code gets protected with a @|swrap| check (@figureref{fig:model:base-interaction}).
@; For boundaries between @|sdeep| and @|suntyped| this is no surprise, because
@;  the @|suntyped| code is unconstrained.
@; For @|sshallow| code, though, static typing provides some checked claims;
@;  one would hope to get away with a less expensive check at the boundary.
@; After all, closed programs that use only @|sdeep| and @|sshallow| code
@;  need no checks in principle because every line of code is validated by the
@;  strong surface-language type checker.
@; 
@; One possible way to optimize is to weaken the boundary between @|sdeep| and
@;  @|sshallow|.
@; @|sDeep| can avoid wrapping an export if the value never interacts with @|suntyped|
@;  code going forward.
@; Likewise, @|sdeep| can trust an import if the value was never handled or influenced
@;  by @|suntyped| code.
@; @Figure-ref{fig:future:opt0} sketches the boundaries that could change via
@;  this strategy; the @|sdeep|--@|suntyped| and @|sshallow|--@|suntyped| boundaries
@;  are unaffected.
@; Note, however, that determining whether a value interacts with @|suntyped|
@;  code requires a careful analysis.
@; Developing a correct analysis that runs quickly is a research challenge in
@;  itself.
@; 
@; @figure*[
@;   "fig:future:opt0"
@;   @elem{With an escape analysis, the @|sdeep|--@|sshallow| boundaries could be weakened.}
@;   fig:opt0]
@; 
@; @figure*[
@;   "fig:future:opt1"
@;   @elem{With an escape analysis and the ability to create wrappers in @|sshallow| code, all runtime type checks could be pushed to the boundaries with @|suntyped| code.}
@;   fig:opt1]
@; 
@; A second possibility is to make the @|sdeep|--@|sshallow| boundary
@;  a @|snoop| by delaying wrappers until a @|sdeep| value reaches @|suntyped| code.
@; Ideally, this strategy can work with an escape analysis to avoid wrapping
@;  @|suntyped| values that never reach @|sdeep| code (@figureref{fig:future:opt1}).
@; The challenge here is to design an escape analysis and to add wrapper-making
@;  code to @|sshallow| without losing the expressiveness that @|stransient| gains
@;  by avoiding wrappers altogether.
@; For first-order interactions,
@;  @|sShallow| can be careful about the identifiers that it sends to @|suntyped| code.
@; Higher-order communication is the real source of difficulties.
@; For example,
@;  if @|sshallow| imports an @|suntyped| map function, then @|sshallow| must be
@;  prepared to wrap every function that it sends to map just in case one of the functions
@;  is @|sdeep|-typed.
@; 
@; If a language can create wrappers in @|sshallow| code, however, then a
@;  @|sforgetful| semantics@~cite{g-popl-2015,gf-icfp-2018}
@;  may be a better fit than @|sTransient|.
@; @|sShallow| types via @|sforgetful| do not require shape checks throughout
@;  typed code, and the 1-level wrappers can dynamically cooperate with @|sdeep|-wrapped
@;  values; that is, the interactions do not require a static analysis
@;  because the wrappers carry information.
@; 
@; A different approach is to adapt the idea of confined types@~cite{afgt-oopsla-2014}.
@; If the type system can prove that a value originates in typed code
@;  and never escapes to untyped, then @|sdeep| and @|sshallow| can freely share
@;  the value.
@; In particular, a @|sshallow| function with a confined-type domain may
@;  not require any shape checks.

