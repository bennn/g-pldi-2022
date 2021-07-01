#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; bring in analogy to GT? typed + untyped ... weak-typed + strong-typed ?

@; ps Greenberg is a 2nd impasse, stat-first vs dyn-first ... worth mentioning?

@title[#:tag "sec:conclusion"]{Conclusion and Future Work}

Gradual typing began with the observation that static and dynamic typing have
complementary strengths@~cite{st-sfp-2006,tf-dls-2006,mf-toplas-2009,gktff-sfp-2006}.
By combining both styles into a mixed-typed language, researchers gave
programmers control over the static/dynamic tradeoff.
But at the same time, the implementation of mixed-typed languages revealed
new tradeoffs along three axis:
the guarantees that types provide,
the run-time cost of enforcing guarantees against untyped code,
and the expressiveness of the mixed-typed language.
A promising solution to these tradeoffs is to give programmers control over the
strength of types.

This paper contributes the first language design that gives programmers the
ability to choose between two forms of sound gradual typing.
The author of a typed module can use either @|sdeep| types as realized by
the @|snatural| semantics or @|sshallow| types as realized by the @|stransient|
semantics.
At run-time, the language ensures the integrity of @|sdeep| and @|sshallow|
types against untyped code and, as needed, against one another.
An implementation of this design in Typed Racket suggests that the combination
is better than either @|sdeep| or @|sshallow| alone in terms of guarantees,
performance, and expressiveness.

@; Knowing now that these extreme designs can interoperate paves the way for
@; other combinations; say between two wrapping semantics.

@; expressiveness is NOT surprising, it's the same argument from POPL 2017

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

A second line of future work is to incorporate a dynamic type
that satisfies the graduality properties@~cite{svcb-snapl-2015}.
For the model, the authors conjecture that @|sdeep| and @|sshallow| soundness
can be achieved by adding a new wrapper for dynamic-typed values and nothing
more.
Implementing the dynamic type may pose new challenges, especially when it
comes to blame and especially if the implementation targets an existing
static language@~cite{g-snapl-2019}.
@; HO HUM ... cut this paragraph?

A third direction for future work is to identify best practices for coding in
a three-way language.
Anecdotal experience suggests that mixing @|sdeep| and @|sshallow| is
a better way to add types to an untyped codebase:
@itemlist[#:style 'ordered
@item{
  Start by adding @|sdeep| types, because their strong guarantees may help
  identify logical errors.
}
@item{
  If performance becomes an issue, switch to @|sshallow| types and continue
  adding @|sshallow| annotations to the next modules.
}
@item{
  Once the codebase is predominantly typed, or once all high-traffic boundaries
  are typed, switch back to @|sdeep| maximize performance.
}
]
@|noindent|The challenge is to rigorously test the effectiveness of this
migration story, and of other intuitions about how to program well in
a gradual language.



@;@section[#:tag "sec:evaluation:perf:release"]{Release Information}
@;
@;@|sShallow| Typed Racket is publicly available in a pull request to Typed Racket:
@;@github-pull["racket" "typed-racket" "948"].
@;The patch adds support for @|sshallow| types, giving Typed Racket programmers
@; a choice between @|sshallow| and @|sdeep| type guarantees.
@;I expect to merge the pull request early in 2021.
@;After the release, I look forward to studying programmers' experience with
@; the multi-faceted system.

@; acks
@; Greenman received support from NSF grant 2030859 to the CRA for the \href{https://cifellows2020.org}{CIFellows} project.


@exact|{\bibliography{bg}}|
