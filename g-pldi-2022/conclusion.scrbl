#lang scribble/acmart
@(require "main.rkt" "bib.rkt")

@; ps Greenberg is a 2nd impasse, stat-first vs dyn-first ... worth mentioning?

@title[#:tag "sec:conclusion"]{Conclusion}
@; {On Language Design}
@; {Let the Programmer Choose}

@; spectrum? fine-grained?

This is the first implementation of a sound gradual type system where
programmers can explicitly choose to trade performance for guarantees
as they add types.
If a new set of type annotations brings unacceptable overhead,
switching the types' semantics from @|sdeep| to @|sshallow|
can avoid the bottleneck and may even be good enough to deploy.
The guarantees from @|sdeep| types can always be used for debugging
the inevitable failure, and can be applied sparingly to defend
a critical module.
In the future, implementors may wish to explore other ways to trade
performance for guarantees, making the trade-off even more programmable.


@; Gradual typing resolves the impasse between static and dynamic typing by
@; bringing both styles under one roof and letting the programmer choose.
@; In the same way, the combination of @|sdeep| and @|sshallow| types resolves an
@; impasse evident in mixed-typed languages.
@; Every sound method for mixed-typed interoperability must
@; balance three dimensions: the guarantees that types provide, the expressiveness
@; of type boundaries, and the cost of run-time checks.
@; Because different choices offer fundamentally different strengths and
@; weaknesses, a promising way forward is to give programmers
@; control over how to enforce gradual types.


@; This paper contributes the first language design that gives programmers the
@; ability to choose between two forms of sound gradual typing.
@; The author of a typed module can use either @|sdeep| types as realized by
@; the @|snatural| semantics or @|sshallow| types as realized by the @|stransient|
@; semantics.
@; At run-time, the language ensures the integrity of @|sdeep| and @|sshallow|
@; types against untyped code and, as needed, against one another.
@; An implementation of this design in Typed Racket suggests that the combination
@; is better than either @|sdeep| or @|sshallow| alone on all three fronts.
@;
@; As a whole, the work reveals insights about the extent to which two typed
@; languages can cooperate in the presence of untyped code.
@; The integration of @|snatural| and @|stransient| demonstrates that extreme
@; combinations are possible.
@; Further systematic efforts are needed to find optimal points in this
@; multi-faceted design space.


@;@section[#:tag "sec:evaluation:perf:release"]{Release Information}
@;
@;@|sShallow| Typed Racket is publicly available in a pull request to Typed Racket:
@;@github-pull["racket" "typed-racket" "948"].
@;The patch adds support for @|sshallow| types, giving Typed Racket programmers
@; a choice between @|sshallow| and @|sdeep| type guarantees.
@;I expect to merge the pull request early in 2021.
@;After the release, I look forward to studying programmers' experience with
@; the multi-faceted system.

@acks{
  This work is supported by NSF grant 2030859 to the CRA for the
  @hyperlink["https://cifellows2020.org"]{CIFellows} project.
  Thanks to Matthias Felleisen for improving drafts of this paper and to
  the rest of my thesis committee for supervising parts of this work:
  Amal Ahmed,
  Fritz Henglein,
  Shriram Krishnamurthi,
  Sam Tobin-Hochstadt,
  and
  Jan Vitek.
}


@exact|{
\begingroup
\setlength{\emergencystretch}{8em}
\printbibliography
\endgroup
}|
