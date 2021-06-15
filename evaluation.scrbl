#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in racket/math
     exact-round)
   (only-in math/statistics
     mean)
   (only-in gtp-plot/performance-info
     performance-info->num-units)
   (only-in "data/analyze.rkt"
     benchmark-name->performance-info
     find-lowest-3dpath-D
     find-lowest-3dpath-D*
     get-3d-table
     get-mixed-path-table
     render-mixed-path-table
     get-mixed-worst-table
     render-mixed-worst-table)
   (only-in "pict.rkt"
     fig:any-wrap
     fig:no-wrap
     fig:index-of
     untyped-codeblock
     typed-codeblock))

@; THESIS Deep + Shallow > Deep | Shallow
@; - sections for guarantees, performance, and expressiveness
@; - is expressiveness incidental?
@;   * Any => Procedure => (f x) ... may require static typing changes to really improve
@; - focus on worst case for performance

@title[#:tag "sec:evaluation"]{Exceeding Expectations}

The integration of @|sDeep| and @|sShallow| Typed Racket offers substantial
benefits over either one alone:
@itemlist[
@item{
  Switching from @|sshallow| to @|sdeep| strengthens the formal guarantees
  for a block of code.
  A one-line change to the @tt{#lang line} improves types from local spot-checks
  to claims that hold throughout the program, including in untyped modules.
}
@item{
  @|sShallow| types can express more programs than @|sdeep| because they
  enforce weaker guarantees.
  The added flexibility enables a surprising number of desirable Typed Racket
  programs (@sectionref{sec:evaluation:expressiveness}).
}
@item{
  Together, the combination of @|sDeep| and @|sShallow| improves the worst-case
  overhead of mixing typed and untyped code (@sectionref{sec:evaluation:performance}).
  In fully-typed programs, @|sdeep| types add no cost and often run faster
  than untyped code because of the type-directed optimizer.
  In mixed-typed programs, @|sshallow| avoids the high overheads that
  can arise from @|sdeep| contracts.
}
]


@section[#:tag "sec:evaluation:expressiveness"]{Guarantees by @|sDeep|}

By design, @|sdeep| types enforce stronger guarantees than @|sshallow|.
A @|sdeep| type is a valid claim about run-time behavior, plain and simple.
No matter where a @|sdeep|-typed value ends up, the type constrains its behavior.
A @|sshallow| type is valid only in a limited scope.
If a value escapes to untyped or less-precisely typed (perhaps via @${\ssubt}) code,
then its @|sshallow| type gets forgotten.
Refer to the example in @section-ref{sec:background} to see how the weak
@|sshallow| constraints can let an erroneous program silently compute a
wrong result.

Although the @|sshallow| semantics is new to Typed Racket, prior work
suggests that its use will occasionally lead to confusing situations.
@citet{lgfd-icfp-2021} performed an automated study of debugging in
Typed Racket and found that @|sShallow| blame errors are more likely to
reach a dead end than @|sDeep| blame errors.
@citet{tgpk-dls-2018} conducted a survey using a hypothetical gradual language
and reported that participants found the @|sshallow| semantics ``unexpected''
more often than the @|sdeep| semantics.
If such confusing situations arise in practice, the ability to change from
@|sshallow| to @|sdeep| types may prove useful for understanding the issue.


@section[#:tag "sec:evaluation:expressiveness"]{Expressiveness by @|sShallow|}
@; - TODO can we fix occurrence typing? Any -> box? -> set-box! ???
@;    ditto for objects, avoid the cast, is it worth changing the typechecker?
@; - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow
@; - higher-order / any errors, gone
@; - indexof ... weird result, gone

Conversations with Typed Racket users have shown that @|sdeep| types can
 lead to unexpected outcomes.
In some programs, type enforcement appears overly strict.
In others, type enforcement is impossible because the implementation of
 @|sDeep| Racket lacks wrappers for certain kinds of values.
Worst of all, the wrappers that @|sDeep| inserts can change hehavior.
@|sShallow| Racket avoids all of these issues because of its weak, wrapper-free
 method of enforcing types.


@subsection{Less-strict Any Type}

@user-inspiration['(
 ("Denis Michiels"
  "error : Attempted to use a higher-order value passed as `Any` in untyped code"
  "2018-04-16"
  "https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ")
 ("Marc Kaufmann"
  "Typed Racket: 'Unable to protect opaque value passed as `Any`' with interesting behavior"
  "2019-12-11"
  "https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ"))]

The @|sdeep| type named @tt{Any} is a normal ``top'' type at compile-time,
 but it is surprisingly strict at run-time.
For compile-time type checking, @tt{Any} is a supertype of every other
 type.
A function that expects an @tt{Any} input must ask occurrence-typing questions
 before it can do anything to it.
At run-time, the @tt{Any} type is enforced with an opaque wrapper.
Refer to @citet{fb-tr-2006} for further discussion of why the opaque wrapper is
 necessary.

@figure*[
  "fig:evaluation:any-wrap"
  @elem{@|sDeep| seals mutable values of type @tt{Any} in a wrapper. @|sShallow| lets untyped code modify the box.}

  fig:any-wrap]

The wrapper is a surprise for developers who expect programs such
 as @figure-ref{fig:evaluation:any-wrap} to run without error.
This program defines a mutable box in typed code,
 assigns the @tt{Any} type to the box,
 and sends it to untyped code.
The untyped module attempts to set the box.
@|sDeep| Racket raises an exception when untyped code tries to modify the box.
Unfortunately for the programmer, this error is essential for soundness.
If untyped code put an integer in the box, then typed uses of the
 box would give a result that is inconsistent with its type.

@|sShallow| Racket runs the program without error because of its delayed
 checking strategy.
If @|sshallow|-typed code tries to read a symbol from the
 box, then that access will raise an error.
Until then, the program runs.


@subsection{No Missing Wrappers}

Every kind of mutable value that can appear in @|sdeep| code needs a kind of
 wrapper to protect it against untyped contexts.
Wrappers do not exist for some values, causing @|sDeep| to reject code
 that sends such a value across a boundary.

@figure*[
  "fig:evaluation:no-wrap"
  @elem{@|sDeep| lacks wrappers for mutable pairs and a few other datatypes. @|sShallow| does not need wrappers, and can express mixed-typed programs that share such values with untyped code.}
  fig:no-wrap]

@Figure-ref{fig:evaluation:no-wrap} demonstrates the issue with a mutable pair
 (@tt{MPairof}) type.
@|sDeep| raises a run-time error when untyped code tries to call the @tt{add-mpair}
 function.
@(let* ((missing-wrapper* '(
          "(Async-Channel T)" "(Custodian-Box T)" "(C-Mark-Key T)" "(Evt T)"
          "(Ephemeron T)" "(Future T)" "(MPair T T')" "(MList T)"
          "(Prompt-Tag T T')" "(Syntax T)" "(Thread-Cell T)" "(Weak-Box T)"))
        (num-missing (length missing-wrapper*)))
  @elem{
In total, there are @integer->word[num-missing] types that
 suffer from this issue.
Implementing wrappers for these types is a challenge.
For example, syntax objects can contain mutable data and therefore need wrappers.
But syntax wrappers would require changes to many parts of the Racket compiler,
 including the macro expander.
})

@|sShallow| Racket avoids the question of how to implement complex wrappers thanks to
 the @|stransient| semantics.
Consequently, programmers gain the ability to send new types across boundaries
 and explore new mixed-typed designs.


@subsection{Uniform Behavior}

@user-inspiration['(
 ("Bertrand"
  "Typed code from untyped code"
  "2020-02-17"
  "https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ")
 ("John B. Clements"
  "index-of + TR ... parametricity problem?"
  "2019-12-15"
  "https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ"))]

Although the purpose of @|sDeep| Racket wrappers is to reject certain operations
 without changing anything else about a program, wrappers can cause some
 programs to run differently.
One obvious case is code that explicitly looks for wrappers; the answers to
 low-level observations such as @tt{has-contract?} may depend on the type
 boundaries in a @|sdeep| program.
@Figure-ref{fig:evaluation:index-of} presents a second, more subtle case.
This typed module imports an untyped function, @tt{index-of}, with a precise
 polymorphic type.
The wrapper that enforces this type
 creates a new wrapper for every input to the function---to enforce parametric
 polymorphism@~cite{gmfk-dls-2007}.
Unfortunately, these input wrappers change the behavior of @tt{index-of};
 it ends up searching the list for a wrapped version of the symbol @tt{'a} and returns
 a ``not found'' result (@tt{#f}) instead of the correct position.

@figure*[
  "fig:evaluation:index-of"
  @elem{The @|sdeep| contract for an @tt{All} type can change the behavior of untyped code.}
  fig:index-of]

@|sShallow| Racket avoids all such changes in behavior,
 including the well-know object identity issues@~cite{stff-oopsla-2012,kt-icfp-2015,vksb-dls-2014,vm-ecoop-2013},
 because the @|stransient| semantics does not use wrappers to enforce types.


@section[#:tag "sec:evaluation:performance"]{Performance by @|sDeep| and @|sShallow|}
@; - worst-case table (can trace "min" line in "fig:transient:overhead"
@; - 
@; - ?? 2-way lattice? 3-way
@; - ?? programs where mix is better than natural-only or transient-only

With the @|sShallow| Racket implementation, the either-or tradeoffs disappear.
For all our benchmarks, the choice improves the worst-case overhead of
 type boundaries.
By implication, Typed Racket can offer a new migration story.

@nested-inset[@emph{
 use @|sshallow| types when converting an untyped application and switch to
 @|sdeep| types after the boundaries stabilize.}]

Mixing @|sdeep| and @|sshallow| types in one program offers new ways of
 improving performance.


@subsection[#:tag "sec:evaluation:perf:worst"]{GTP Benchmarks, Worst-Case}

@(define SHALLOW-CURRENT-BENCHMARK* '(
  sieve forth fsm fsmoo mbta morsecode zombie dungeon jpeg zordoz lnm
  suffixtree kcfa snake take5 acquire tetris synth gregor quadT quadU))


@(let* ((WT (get-mixed-worst-table SHALLOW-CURRENT-BENCHMARK*))
       )
@list[
@figure*[
  "fig:evaluation:mixed-worst-table"
  @elem{
   Worst-case overhead before (@|sdeep| types)
    and after (either @|sdeep| or @|sshallow|)
    the integration of @|sDeep| and @|sShallow| Racket.
  }
  @render-mixed-worst-table[WT]
]
@elem{
Now that Racket programmers can easily switch between @|sdeep| and @|sshallow|
 types, worst-case overheads improve by orders of magnitude.
Before, the cost of @|sdeep| types overwhelmed many configurations.
After, the costs can be avoided by changing the first line (the language specification)
 of the typed modules.

@Figure-ref{fig:evaluation:mixed-worst-table} quantifies the improvements in the
 Typed Racket benchmarks.
The first data column reports the old worst-case overheads.
The second columns reports the new worst-case, now that programmers can
 pick the best of @|sdeep| and @|sshallow| types.
The final column is the quotient between the first two.
In short, the ``after'' case is always better and can be an arbitrarily large
 improvement.
}])



@subsection[#:tag "sec:evaluation:perf:both"]{Case Studies: @|sDeep| and @|sShallow|}
@; Are there any mixed lattice points, using guarded and transient, that do
@;  better than a "pure" configuration?
@; For a negative answer, need a lattice on top of every lattice point.
@; Can try small benchmarks.... ok, then extrapolate.

Early experience with @|sShallow| Racket shows that the combination of
 @|sdeep| and @|sshallow| types can be better that either alone.
Here are three motivating case studies.


@parag{synth}
@(let* ((synth-url "http://github.com/stamourv/synth")
        (synth-data
         (hash
           'd-lib-d-client '(809 821 834 771 733)
           'd-lib-u-client '(11440 11040 11004 11923 11672)
           's-lib-u-client '(1645 1664 1558 1576 1539)
           's-lib-d-client '(7823 7885 9002 7955 8279)))
        (deep-delta (exact-round (/ (mean (hash-ref synth-data 'd-lib-u-client))
                                    (mean (hash-ref synth-data 'd-lib-d-client)))))
        (shallow-delta (exact-round (/ (mean (hash-ref synth-data 's-lib-d-client))
                                       (mean (hash-ref synth-data 's-lib-u-client)))))
        (ds-fast (exact-round (/ (mean (hash-ref synth-data 's-lib-u-client))
                                 (mean (hash-ref synth-data 'd-lib-d-client)))))
        (ds-slow-sec (quotient
                       (- (mean (hash-ref synth-data 'd-lib-u-client))
                          (mean (hash-ref synth-data 's-lib-d-client)))
                       1000))
        (ds-slow (rnd (/ (mean (hash-ref synth-data 'd-lib-u-client))
                         (mean (hash-ref synth-data 's-lib-d-client))))))
  @elem{
The @bm{synth} benchmark is derived from @hyperlink[synth-url]{an untyped program}
 that interacts with part of a typed math library.
When the library code uses @|sdeep| types, the original client runs with
 high overhead---@~a[deep-delta]x slower that a @|sdeep|-typed client.

Changing the library to use @|sshallow| types improves
 the gap between an untyped and @|sdeep|-typed client to
 @~a[shallow-delta]x.
This fast untyped configuration is about @~a[ds-fast]x slower than the fast
 @|sdeep|-@|sdeep| configuration, but the worst-case is @~a[ds-slow]x
 faster (@~a[ds-slow-sec] seconds) than before.
Overall, the @|sshallow| library is a better tradeoff for @bm{synth}.
})

@; if we have a few of these, organize into a figure with descriptions below
@; ... or use a "benchmarks" format to convey the bottom line
@parag{MsgPack}
@hyperlink["http://msgpack.org/"]{MessagePack} is a serialization format.
@hyperlink["https://gitlab.com/HiPhish/MsgPack.rkt"]{MsgPack} is a Typed Racket
 library that maps Racket values to binary data according to the format.
The author of this library
 @hyperlink["https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ"]{reported a performance hit}
 after narrowing some types from @tt{Any} to a more-precise union type for serializable inputs.
Tests that formerly passed on the package server timed out after the change.

I cloned MsgPack commit @github-commit["HiPhish" "MsgPack.rkt"]{64a60986b149703ff9436877da1dd3e86c6e4094}
 and found that running all unit tests took 320 seconds.
Changing one file to @|sshallow| types brought the time down to 204 seconds---a
 huge improvement for a one-line switch.
Moving the rest of the library from @|sdeep| to @|sshallow| types adds only a slight
 improvement (down to 202 seconds), which suggests that a mix of @|sdeep| and
 @|sshallow| is best.

@; can do even better after changing the code:
@;  Deep, no pack casts = 117 seconds
@;  Shallow, no pack casts = 67 seconds
@;  untyped = 24 seconds!!!


@parag{External Data}
Typed code that deals with data from an external source is often better off
 with @|sshallow| types because they lazily validate data as it is accessed.
By contrast, Typed Racket's implementation of @|sdeep| types eagerly
 traverses a data structure as soon as it reaches a type boundary.
If the boundary types allow mutable values, then the traversal is even more
 expensive because it creates wrappers as it copies the dataset.

@(let* ((script_name "QA/transient-expressive/json/custom.rkt")
        (s* '(169 157 162 159 162))
        (t* '(3007 2991 2920 3096 3308))
        (t/s (/ (mean t*) (mean s*)))
        (slowdown (if (< 10 t/s) "over 10x" (format "~ax" (rnd t/s)))))
  @elem{
To illustrate the pitfall, I wrote a typed script that reads a large dataset of
 apartment data using on off-the-shelf JSON parser and accesses one field
 from each object in the dataset.
@|sDeep| types make the script run @|slowdown| slower than @|sshallow| types.
})

In principle, @|sdeep| code can avoid the slowdown with a custom parser
 that validates data as it reads it.
Indeed, Phil Nguyen has written a @hyperlink["https://github.com/philnguyen/json-type-provider"]{library}
 for JSON that mitigates the overhead of @|sdeep| types.
Such libraries are ideal, but until we have them for the next data exchange
 format (SQL, XML, YAML, ...) @|sshallow| types get the job done with the parsers
 that are available today.

@parag{Migration Paths}

@(let* ([bm* '(sieve forth fsm fsmoo mbta morsecode
               zombie dungeon jpeg zordoz lnm suffixtree
               kcfa snake take5)]
        [D 3]
        [PT (get-mixed-path-table D bm*)]
        [deep-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (caddr path-info))) (car path-info))]
        [shallow-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr path-info))) (car path-info))]
        [mix-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr (cdr path-info)))) (car path-info))]
        [_md (unless (= 1 (length mix-dead*)) (printf "HELP expected one mix-dead row got ~s~n" mix-dead*))]
        [mix-path-bm 'fsm]
        [mix-best-D
         ;; TODO apply to more benchmarks?
         (find-lowest-3dpath-D mix-path-bm)]
        [mix-best-D*
         (find-lowest-3dpath-D*
           '(forth fsm fsmoo mbta morsecode zombie dungeon
             jpeg zordoz lnm suffixtree kcfa snake take5))]
        )
@list[
@figure*[
  "fig:both:mixed-path"
  @elem{Percent of @ddeliverable[D] paths in three lattices:
   the @|sdeep|-typed lattice, the @|sshallow|-typed lattice,
   and a hybrid that chooses the best of @|sdeep| or @|sshallow| types at each point.}
  @render-mixed-path-table[PT]
]
@elem{
@|sShallow| types make step-by-step migration more practical in Typed Racket.
Originally, with @|sdeep| types, a programmer who adds types one module at
 a time is likely to hit a performance wall; that is, a few configurations
 along the migration path are likely to suffer a large overhead.
Adding more @|sdeep| types is a sure way to reduce the overhead,
 especially if the programmer adds the best-possible types (@figure-ref{fig:example-path}),
 but these multi-step pitfalls contradict the promise of migratory typing.
High overhead makes it hard to tell whether the new types are compatible with
 the rest of the codebase.

By choosing @|sdeep| or @|sshallow| types at each point along a path, the
 worst-case overhead along migration paths goes down.
@Figure-ref{fig:both:mixed-path} quantifies the improvement by showing the
 percent of all paths that are @ddeliverable[D] at each step.
With @|sdeep| types alone, all paths in @integer->word[(length deep-dead*)]
 benchmarks hit a point that exceeds the @~a[D]x limit.
With @|sshallow| types alone, all paths in @integer->word[(length shallow-dead*)] benchmarks
 exceed the limit as well.
With the mix, however, only @integer->word[(length mix-dead*)] benchmark (@bm[(car mix-dead*)])
 has zero @ddeliverable[D] paths.
Fine-grained combinations of @|sdeep| and @|sshallow| types can further improve
 the number of viable migration paths.
In @bm[mix-path-bm], for example, every path is @ddeliverable[mix-best-D] if the programmer
 picks the fastest-running mix of @|sdeep| and @|sshallow| types for each configuration.

Others look good too @~s[mix-best-D*].
}])

@parag{Case Study: GTP Benchmarks}

@(let* ((DDD (get-3d-table '(forth fsm fsmoo mbta morsecode zombie dungeon
                             jpeg zordoz lnm suffixtree kcfa snake take5
                             acquire tetris )))
        (num-DDD (length DDD))
        (S (benchmark-name->performance-info 'fsm default-rkt-version))
        (fsm-num-modules (performance-info->num-units S))
        (fsm-num-configs (expt 2 fsm-num-modules))
        (fsm-non-mixed (+ 1 fsm-num-modules))
        (fsm-mixed (- fsm-num-configs fsm-non-mixed)))
@list[
@elem{
  For @integer->word[num-DDD] small benchmarks, I measured the full
   space of @${3^N} configurations that can arise by combining @|sdeep|
   and @|sshallow| types.
  Each configuration ran successfully, affirming that @|sdeep| and @|sshallow|
   can interoperate.
  Furthermore, a surprising percent of all @${2^N} mixed-typed configurations
   in each benchmark ran fastest using a mixture of @|sdeep| and @|sshallow|
   types:
}
@(apply itemlist
   (for/list ((d-row (in-list DDD))
              (i (in-naturals 1)))
     (item (format "~a% of " (cadr d-row))
           (bm (car d-row))
           (format " configurations~a"
                   (cond
                    [(= i num-DDD)
                     "."]
                    [(= (+ i 1) num-DDD)
                     "; and"]
                    [else
                     ";"])))))
@elem{
@|noindent|In @bm{fsm}, for example, there are @integer->word[fsm-num-configs] mixed-typed configurations.
@Integer->word[fsm-non-mixed] of these cannot mix @|sdeep| and @|sshallow|
 because they contain at most one typed module.
Of the remaining @~a[fsm-mixed] configurations, over half run fastest with a
 combination of @|sdeep| and @|sshallow| types.
}
])


