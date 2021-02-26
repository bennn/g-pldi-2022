#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS Deep + Shallow is better than Deep or Shallow
@;  - saves guarantees, performance, and expressiveness
@;  - TODO is expressiveness incidental?
@;  - maybe add constraints = no new compiler, no new language

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

@; RQ is it possible?
@; RQ any benefits?

@; ps Greenberg is a 2nd impasse, stat-first vs dyn-first ... worth mentioning?

@title[#:tag "sec:introduction"]{A Way Out}

@; awkward, superficial success

Gradual typing is something of a disappointing success.
On one hand, the research has clearly made an impact.
Several industry teams have developed gradual languages,
 and thousands of programmers use these languages to incrementally
 improve large software projects.
On the other hand, the impact is merely static.
Industry languages have adapted designs for a dynamic type
 and methods for imposing types on an untyped language.
But the adaptations ignore the main research contribution;
 namely, how types can guide interactions among typed and untyped blocks
 of code.
If untyped JavaScript sends an object to a TypeScript application,
 the object can invalidate type assumptions.
Types help industry programmers write new code, but say nothing about
 how that code interacts with the rest of the application.

There is good reason that industry teams have been slow to adopt sound
 gradual typing: performance.
Potentially huge costs.
But inspired many efforts, situation now is much better than five years ago.
Alas competing designs emerged.
In fact, mixed-typed languages must navigate a tradeoff along three dimensions:
 the soundness guarantees that types provide;
 the expressiveness of the type system, in particular at typed/untyped boundaries;
 and the performance cost of enforcing a guarantee.
Typed Racket has strong guarantees, strong expressiveness, and potentially
 awful performance.
Reticulated Python has weak guarantees, strong expressiveness, and
 better worst-case costs.
By sacrifing guarantees, optional languages run no worse than untyped code.
@; bg: may remove this paragraph, but try your best for now


Current alternatives.
Forgo compositional guarantees 
Switch to new language or new compiler technology.
If only could interoperate different gradual semantics.
@; hey what is a gradual semantics yet?

This paper contributes a way to resolve the dilemma without new compiler
 without new language.
The Natural Deep types and Transient Shallow types can work together.
Doing so offers concrete benefits.


Contributions.

