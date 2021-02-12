#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS Deep + Shallow is better than Deep or Shallow
@;  - saves guarantees, performance, and expressiveness
@;  - is expressiveness incidental?
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

@title[#:tag "sec:introduction"]{A Way Out}

Gradual typing is stuck in an awkward position.
On one hand, the basic research idea is a success.
Several companies have adapted gradual type system designs to specific
 untyped languages@~cite[cvgrl-oopsla-2017],
 @; TODO cite more
 and thousands of programmers use these mixed-typed languages to
 incrementally improve large software projects.
On the other hand, the key technical contribution is not a mainstream success.
Research languages show that mixed types can be sound in the face of untyped
 code, but none of the widely-used implementations offer a soundness guarantee.
At best, such languages are sound only for closed-world typed code.

To illustrate the benefits of sound interaction, picture an untyped codebase
 that represents a social network using objects to represent people and
 a graph structure to record links among users.
In any mixed-typed language, programmers can reuse the untyped code and
 introduce types that describe the existing APIs and objects.
Future maintainers can use these types to write new code, such as a function
 @tt{add_sponsor} that expects two @tt{Person} objects and creates a business
 relationship.
But there may be trouble if untyped code can access the @tt{add_sponsor} function.
An unsound language does not promise that function inputs match the @tt{Person}
 type, and so a malformed call may result in either an error or a nonsensical
 result that corrupts the underlying graph.
In a sound mixed-typed language, programmers have some guarantee that inputs
 match the types.

Guarantees, though, are not free.
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

Tradeoff has been quite an issue.
Research looks stuck.
Alas.

In this paper we explore.

Contributions.



@; Greenberg is a 2nd impasse ... worth mentioning?


