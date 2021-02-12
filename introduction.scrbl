
@; TODO model TransientExperience for scribble

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

@section[#:tag "sec:introduction"]{A Way Out}

Gradual typing is stuck in an awkward position.
On one hand, the basic research idea is a success.
Several companies have adapted gradual type system designs to specific
 untyped languages@~cite[ts],
 and thousands of programmers use these mixed-typed languages to
 incrementally improve large software projects.
On the other hand, the key technical contribution is a failure.
Research languages show that retrofitted types can make guarantees
 about the behavior of untyped code,
 but these designs for sound interaction have not made an impact.

To illustrate the benefits of sound interaction, imagine an untyped codebase
 that represents a social network using objects to represent people and
 a graph structure to record personal relationships.
In a mixed-typed language, programmers can wrap the untyped code in a layer of
 type definitions to help future maintainers.
alas


 codebase that represents a social network.
In particular, the untyped code defines objects that represent people and
 maintains a graph of personal relationships.
With gradual types, a programmer can keep the untyped code as-is and add
 type definitions 

wrap the untyped code in a typed
 interface to 

With a mixed-typed extension, programmers can add type definitions to
 describe 

In a mixed-typed language, programmers can use types to document the untyped
 code without 

In a mixed-typed language, programmers can introduce types that describe


On one hand, journalists hail the research area as a great success
 because thousands of programmers currently use mixed-typed languages
 to add discipline to untyped code@~cite[].
The type systems of these languages are furthermore industry adaptations of
 academic ideas@~cite[]---a headline-perfect example of technology transfer.
On the other hand, none of the mainstream languages offer guarantees
 about the @emph{interactions} between typed and untyped code.
The central research contribution has not yet made an impact.

There is one exception: Dart 2.

@; Erasure vs. others? Where to add ... academic works?
In short, languages such as TypeScript offer a static analysis for
 closed-world typed code.
Consider a typed function @tt{add_subscriber} that expects two @tt{User} objects
 and creates a link between them.
If untyped code has access to this function,
 perhaps because @tt{add_subscriber} is a new addition to an untyped codebase,
 then the function can receive any sort of input at runtime.
Research on gradual typing shows how to build a sound static analysis
 using run-time checks.
 


@; Greenberg is a 2nd impasse ... worth mentioning?


