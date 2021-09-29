#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; bring in analogy to GT? typed + untyped ... weak-typed + strong-typed ?

@; ps Greenberg is a 2nd impasse, stat-first vs dyn-first ... worth mentioning?

@title[#:tag "sec:conclusion"]{Let The Programmer Choose}

Gradual typing resolves the impasse between static and dynamic languages by
bringing both styles under one roof and letting programmer choose.
In the same way, combining @|sdeep| and @|sshallow| types resolves an
impasse among mixed-typed languages.
The problem is that every sound method of typed--untyped interoperability must
balance three dimensions: the guarantees that types provide, the expressiveness
of type boundaries, and the cost of run-time checks.
Different balances offer distinct strengths and weaknesses, but no existing
language lets programmers combine type-enforcement strategies.

This paper contributes the first language design that gives programmers the
ability to choose between two forms of sound gradual typing.
The author of a typed module can use either @|sdeep| types as realized by
the @|snatural| semantics or @|sshallow| types as realized by the @|stransient|
semantics.
At run-time, the language ensures the integrity of @|sdeep| and @|sshallow|
types against untyped code and, as needed, against one another.
An implementation of this design in Typed Racket suggests that the combination
is better than either @|sdeep| or @|sshallow| alone on all three fronts.

As a whole, the work reveals insights about the extent to which two typed
languages can cooperate in the presence of untyped code.
The integration of @|snatural| and @|stransient| demonstrates that extreme
combinations are possible.
Further systematic efforts are needed to find optimal points in this
multi-faceted design space.


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
