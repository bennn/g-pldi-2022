#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in racket/math
     exact-round)
   (only-in math/statistics
     mean
     median)
   (only-in gtp-plot/performance-info
     performance-info->num-units)
   (only-in "data/analyze.rkt"
     benchmark-name->performance-info
     find-lowest-3dpath-D
     find-lowest-3dpath-D*
     get-3d-table
     render-3d-table-y
     get-mixed-path-table
     render-mixed-path-table
     get-mixed-worst-table
     render-mixed-worst-table)
   (only-in "pict.rkt"
     fig:any-wrap
     fig:no-wrap
     fig:index-of
     untyped-codeblock
     typed-codeblock)
   (only-in scriblib/footnote note))

@; THESIS Deep + Shallow > Deep | Shallow
@; - sections for guarantees, performance, and expressiveness
@; - is expressiveness incidental?
@;   * Any => Procedure => (f x) ... may require static typing changes to really improve
@; - focus on worst case for performance

@title[#:tag "sec:evaluation"]{Evaluation}

The integration of @|sDeep| and @|sShallow| Typed Racket offers substantial
benefits over either one alone:
@itemlist[
@item{
  Switching from @|sShallow| to @|sDeep| strengthens the formal guarantees
  for a block of code.
  A one-line change to the @tt{#lang} line improves types from local spot-checks
  to claims that hold throughout the program, including in untyped modules.
}
@item{
  @|sShallow| types can express more programs because they
  enforce fewer constraints.
  The added flexibility enables a number of desirable Typed Racket
  programs (@sectionref{sec:evaluation:expressiveness}).
}
@item{
  Together, the combination of @|sDeep| and @|sShallow| improves the worst-case
  overhead of mixing typed and untyped code (@sectionref{sec:evaluation:performance}).
  In fully-typed programs, @|sdeep| types add no cost and often run faster
  than untyped code because of the type-directed optimizer.
  In mixed-typed programs, @|sshallow| types avoid the high overheads that
  can arise from contracts.
}
]


@section[#:tag "sec:evaluation:guarantees"]{Guarantees by @|sDeep|}

By design, @|sdeep| types enforce stronger guarantees than @|sshallow|.
A @|sdeep| type is a claim about run-time behavior that is substantiated
by comprehensive run-time checks.
No matter where a @|sdeep|-typed value ends up, the type constrains its behavior.
A @|sshallow| type is valid only in a limited scope.
If a value escapes to untyped or less-precisely typed (perhaps via @${\ssubt}) code,
then its @|sshallow| type gets forgotten.
Refer to the example in @section-ref{sec:background} to see how the weak
@|sshallow| constraints can have no impact on the behavior of a program.

Although the @|sshallow| semantics is new to Typed Racket, prior work
suggests that its use will occasionally lead to confusing situations.
@citet{lgfd-icfp-2021} performed an automated study of debugging in
Typed Racket and found that @|sShallow| blame errors are more likely to
reach a dead end than @|sDeep| blame errors.
@citet{tgpk-dls-2018} conducted a survey using a hypothetical gradual language
and reported that participants found the @|sshallow| @|stransient| semantics ``unexpected''
more often than the @|sdeep| @|snatural| semantics.
If such confusing situations arise in practice, the ability to change from
@|sshallow| to @|sdeep| types may prove useful for comprehending the root issue.


@section[#:tag "sec:evaluation:expressiveness"]{Expressiveness by @|sShallow|}
@; - TODO can we fix occurrence typing? Any -> box? -> set-box! ???
@;    ditto for objects, avoid the cast, is it worth changing the typechecker?
@; - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow
@; - higher-order / any errors, gone
@; - indexof ... weird result, gone

@|sShallow| Racket can express useful programs that @|sDeep| Racket
halts with a run-time error.
At first glance, the existence of such programs is surprising because
the theory (@section-ref{sec:model}) suggests that the @|sdeep| semantics
is more ``correct'' than @|sshallow|.
It turns out that the @|sdeep| semantics for certain types is very restrictive,
to the point that delayed @|sshallow| checks work better in practice
(@section-ref{sec:evaluation:expr:any}).
In other programs, the gap between @|sDeep| and @|sShallow| Racket is due
technical challenges regarding higher-order contracts.
Language designers should be aware of these issues when building a new
@|sdeep| semantics.


@subsection[#:tag "sec:evaluation:expr:any"]{Less-strict Top Type}

The @|sDeep| Racket type named @tt{Any} is a normal top type at compile-time,
but has a strict semantics at run-time.
For compile-time type checking, @tt{Any} is a supertype of every other type.
At run-time, the @tt{Any} type is enforced with an opaque wrapper that
prohibits a client from inspecting the underlying value@~cite{fb-tr-2006}.

The opaque wrapper ensures full type soundness, but can be overly restrictive
in programs such as the one shown in @figure-ref{fig:evaluation:any-wrap}.
This program defines a mutable box of symbols in typed code,
 assigns the @tt{Any} type to the box,
 and sends it to untyped code.
The untyped module writes a symbol to the box.
@|sDeep| Racket rejects this harmless write, but @|sShallow| Racket allows it.

Other top types for higher-order values can lead to similar programs.
For example, @|sShallow| can import a function at the general @tt{Procedure} type,
cast it to a more specific type, and try an application.
The application may succeed if the specific type is correct.
A @|sDeep| cast simply adds a second wrapper on top of the
restrictive wrapper for the @tt{Procedure} type.@note{Because
@|sdeep| program can do so little with a @tt{Procedure} value, library authors
must use clever types to define generic utility functions for procedures.
Until recently, the @tt{object-name} function had a useless type
(@github-commit["racket" "typed-racket" "47a5ab3e2f335e6956aea4b98700d22a359ad6b2"]).}


@subsection[#:tag "sec:evaluation:expr:wrap"]{No Missing Wrappers}

Every kind of mutable value that can appear in @|sdeep| code needs a tailored
wrapper to protect it against untyped contexts.
These wrappers are difficult to implement because they require support from the
run-time system@~cite{stff-oopsla-2012}.
Unsurprisingly, some infrequently-used types lack wrappers.

@Figure-ref{fig:evaluation:no-wrap} demonstrates the issue with a mutable pair
(@tt{MPairof}) type.
@|sDeep| Racket raises a run-time error when untyped code tries to call the @tt{add-mpair}
 function, but @|sShallow| can run the program.
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


@|sShallow| Racket avoids the question of how to implement complex wrappers
thanks to the @|stransient| semantics.
A @|sshallow| language must decide how to implement first-order checks,
but these require far less engineering than behavioral wrappers.


@subsection[#:tag "sec:evaluation:expr:uniform"]{Uniform Behavior}

Although the purpose of @|sDeep| Racket wrappers is to reject certain operations
without otherwise changing the behavior of program, wrappers can cause some
programs to run differently.
One obvious case is code that explicitly looks for wrappers; the answers to
low-level observations such as @tt{has-contract?} may depend on the type
boundaries in a @|sDeep| Racket program.
@Figure-ref{fig:evaluation:index-of} presents a second, more subtle case.
This typed module imports an untyped function, @tt{index-of}, with a precise
 polymorphic type.
The wrapper that enforces this type
 creates a new wrapper for every input to the function---to enforce parametric
 polymorphism@~cite{gmfk-dls-2007}.
Unfortunately, these input wrappers change the behavior of @tt{index-of};
 it ends up searching the list for a wrapped version of the symbol @tt{'a} and returns
 a ``not found'' result (@tt{#f}) instead of the correct position (@tt{0}).

@|sShallow| Racket avoids all such changes in behavior,
 including the well-know object identity issues@~cite{stff-oopsla-2012,kt-icfp-2015,vksb-dls-2014,vm-ecoop-2013},
 because the @|stransient| semantics does not use wrappers to enforce types.


@section[#:tag "sec:evaluation:performance"]{Performance by @|sDeep| and @|sShallow|}
@; - worst-case table (can trace "min" line in "fig:transient:overhead"
@; - 
@; - ?? 2-way lattice? 3-way
@; - ?? programs where mix is better than natural-only or transient-only

@(define SHALLOW-CURRENT-BENCHMARK* '(
  sieve forth fsm fsmoo mbta morsecode zombie dungeon jpeg zordoz lnm
  suffixtree kcfa snake take5 acquire tetris synth gregor quadT quadU))

The three-way mix of @|sdeep| and @|sshallow| types improves performance
across the board.
On the GTP benchmark suite,@note{GTP Benchmarks version 6.0, @|gtp-url|}
toggling between @|sdeep| and @|sshallow| avoids the
pathological cases in each.
Mixing @|sdeep| and @|sshallow| modules can further improve performance
in many programs.

The data that this section reports was collected on a single-user Linux box
with @~a[NSA-num-cores] physical @~a[NSA-core-name] @~a[NSA-core-speed] cores and @~a[NSA-RAM] RAM.
The specific languages involved were Racket v7.8.0.5
(@github-commit["racket" "racket" "7c903871bd8cb4bd32ed7188c180b5124f9bc201"]).
and a pre-release of Shallow Racket (@github-commit["bennn" "typed-racket" "c074c9333e467cb7cd2058511ac63a1d51b4948e"])
that extends Typed Racket v1.12.
@; no need for a TR commit, sends the wrong message --- reproductions need not check that out


@subsection[#:tag "sec:evaluation:perf:either-or"]{Worst-Case Overhead vs. Untyped}

@(let* ((WT (get-mixed-worst-table SHALLOW-CURRENT-BENCHMARK*))
       )
@list[
@figure[
  "fig:evaluation:mixed-worst-table"
  @elem{
   Worst-case overheads vs. the untyped configuration for @|sDeep| alone, @|sShallow| alone, and an either-or mix.
  }
  @render-mixed-worst-table[WT]
]
@elem{
The literature on gradual typing reports bottlenecks for both @|sdeep|
and @|sshallow| implementations.
With @|sdeep| types, frequent boundary-crossings can result in a huge
cost@~cite{htf-hosc-2010,tfgnvf-popl-2016,gtnffvf-jfp-2019}.
With @|sshallow| / @|stransient|, every line of typed code may add a small cost
due to shape checks@~cite{vss-popl-2017,gm-pepm-2018}.
These bottlenecks are indeed present and problematic in Typed Racket.

Fortunately for the combined language, the bottlenecks of @|sdeep| and
@|sshallow| types are often complementary.
Consider a program with @${N} modules and therefore @${2^N} @emph{configurations}
in which a subset of these modules are untyped and the rest are typed.
@|sDeep| suffers in configurations that frequently send values across type boundaries,
but can run faster than untyped when all @${N} modules have types.
@|sShallow| performance degrades as the number of typed modules
increases.
Using @|sShallow| initially and @|sDeep| once all critical boundaries are
typed is therefore an easy way to avoid the worst cases of each.

@Figure-ref{fig:evaluation:mixed-worst-table} quantifies the benefits of
switching between @|sDeep| and @|sShallow| Racket on the GTP benchmarks.
The first column shows that, as expected, @|sDeep| can lead to
enormous run-time costs.
The second column shows that the bottlenecks for @|sShallow| Racket are
far less severe.
The third column shows, however, that
toggling between @|sDeep| and @|sShallow| (instead of mixing both
in one program) can do even better.
Numbers in this third column are typeset in bold if they provide evidence
of complementary strengths; that is, if the third column reports a lower
overhead than the first and second columns.
Although some benchmarks have overlapping bottlenecks,
most benefit from @|sDeep| in certain configurations and @|sShallow| in others.
The @tt{zombie} benchmark is a notable failure; @|sShallow| pays a high
cost because one module contains a large number of elimination forms,
and the @|sDeep| boundaries are even more costly.
The @tt{sieve} and @tt{tetris} benchmarks are the best successes.
}])


@subsection[#:tag "sec:evaluation:perf:path"]{Migration Paths}

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
        [path-3d-D 1.2]
        [path-3d
         (for/list ((bmx (in-list mix-best-D*))
                    #:when (and (cdr bmx) (<= (cdr bmx) path-3d-D)))
           (bm (car bmx)))]
        )
@list[
@figure[
  "fig:both:mixed-path"
  ;; TODO barchart? yes yes yes
  ;; TODO if table, highlight rows where D||S wins
  @elem{
    Percent of @ddeliverable[D] paths for @|sDeep| alone, @|sShallow| alone, and an either-or mix.
  }
  @render-mixed-path-table[PT]
]
@elem{
The complementary strengths of @|sDeep| and @|sShallow| Racket can help
programmers quickly migrate an untyped codebase toward a more-typed configuration.
The reason is because switching between the two often performance bottlenecks.

In a program with @${N} modules, a migration path is a sequence of
@${N\!+\!1} configurations.
The first element of a path must be the untyped configuration.
Each successive element adds types to one new module.
A @ddeliverable{D} migration path has the additional constraint that each
configuration runs at most @${D} times slower than the untyped configuration.

@Figure-ref{fig:both:mixed-path} counts the proportion of @ddeliverable{3}
paths out of all @${N!} migration paths in a subset of the GTP benchmarks.
Larger benchmarks are omitted.
The first column counts paths in @|sDeep| Racket,
the second column counts paths in @|sShallow| Racket, and
the third column counts paths using @|sDeep| or @|sShallow| at each point.
With @|sDeep| alone, all paths in @integer->word[(length deep-dead*)]
benchmarks reach a bottleneck that exceeds the @~a[D]x limit.
With @|sShallow| alone, all paths in @integer->word[(length shallow-dead*)] benchmarks
 exceed the limit as well---typically near the end of the migration path.
With the either-or mix, only @integer->word[(length mix-dead*)] benchmark (@bm[(car mix-dead*)])
 has zero @ddeliverable[D] paths.

Additional paths become @ddeliverable{D} for low values of @${D} when
configurations are allowed to use a combination of @|sdeep| and @|sshallow|
modules.
All paths in @oxfordize[path-3d] are @ddeliverable[path-3d-D]
with fine-grained mixtures.
That said, the authors found these mixtures through a brute-force search
of @${3^N} configurations.
It remains to be seen whether a heuristic can lead programmers to useful
combinations in practice.
}])


@subsection[#:tag "sec:evaluation:perf:together"]{Three-way Case Studies}
@; three-way excellence ?

@(let* ((DDD (get-3d-table '(forth fsm fsmoo mbta morsecode zombie dungeon
                             jpeg zordoz lnm suffixtree kcfa snake take5
                             acquire tetris )))
        (num-DDD (length DDD))
        (DDD-n* (map (lambda (x) (string->number (cadr x))) DDD))
        (DDD-mean (mean DDD-n*))
        (DDD-median (median < DDD-n*))
        (S (benchmark-name->performance-info 'fsm default-rkt-version))
        (fsm-num-modules (performance-info->num-units S))
        (fsm-num-configs (expt 2 fsm-num-modules))
        (fsm-non-mixed (+ 1 fsm-num-modules))
        (fsm-mixed (- fsm-num-configs fsm-non-mixed)))
@list[
@elem{
Mixing @|sdeep| and @|sshallow| types within one program can improve its
performance.
@|sDeep| types are best among a tightly-connected group of modules that
rarely interact with untyped or @|sshallow|-typed code.
@|sShallow| types run fastest in modules that act as a bridge to
untyped code and do little computation on their own.
When a program contains typed modules that fill each kind of role, a combination
of @|sdeep| and @|sshallow| runs better that either alone.

Such programs arise often in the GTP benchmarks (@figure-ref{fig:both:3way}).
Out of the @${2^N} configurations in @integer->word[num-DDD] of the smaller
benchmarks, a median of @$[@~a[DDD-median] "\\%"] run fastest with a mix of
@|sDeep| and @|sShallow|.
These results are especially encouraging because @${N\!+\!1} configurations in
each benchmark cannot mix @|sdeep| and @|sshallow| because they contain fewer
than @${2} typed modules.
In @bm{fsm}, for example, there are @integer->word[fsm-num-configs] mixed-typed configurations.
@Integer->word[fsm-non-mixed] of these have at most one typed module.
Of the remaining @~a[fsm-mixed] configurations, over half run fastest with a
combination of @|sdeep| and @|sshallow| types.

The challenge, however, lies in finding which typed modules ought to be @|sdeep|
and which ought to be @|sshallow|.
@Figure-ref{fig:both:3way} is the result of an exhaustive search in a
@${3^N} lattice.
FILL then again the best 3-way mix is only 2x faster than the best either-or, so that's probably enough.
In practice, developers may not be willing to run a search to find the best
combinations.
Further experience with @|sdeep| and @|sshallow| types may
reveal best practices for finding an efficient mix.
As a first step, the following three case studies report on manual searches.
}
@figure[
  "fig:both:3way"
  ;; TODO nicer symbol than + for the mix? ... || is great for either-or ... \cup vs \uplus ???
  @elem{
    Percent of configurations that run fastest with a mix of @|sDeep| and @|sShallow| modules.
  }
  (render-3d-table-y DDD)]
])

@parag{Synth}
@(let* ((synth-short-url "github.com/stamourv/synth")
        (synth-url (string-append "http://" synth-short-url))
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
The GTP benchmark named @bm{synth} is based on @hyperlink[synth-url]{an untyped program}
that interacts with a typed math library to synthesize music.@note{@shorturl["https://" synth-short-url]}
When the math library uses @|sDeep| Racket, the original client runs with
high overhead.
Compared to a @|sDeep| client program, the untyped client
suffers a @~a[deep-delta]x slowdown.

Changing the library to use @|sShallow| Racket improves the gap between an
untyped and @|sDeep| client to @~a[shallow-delta]x.
This fast untyped configuration is roughly @~a[ds-fast]x slower than the fast
@|sDeep|-@|sDeep| configuration, but the worst-case is @~a[ds-slow]x
faster (@~a[ds-slow-sec] seconds) than before.
Overall, a @|sShallow| version of the math library is a better tradeoff for @bm{synth}.
})

@parag{MsgPack}
@hyperlink["http://msgpack.org/"]{MessagePack} is a serialization format.
@hyperlink["https://gitlab.com/HiPhish/MsgPack.rkt"]{MsgPack} is a Typed Racket
 library that maps Racket values to binary data.@note{@shorturl["https://" "gitlab.com/HiPhish/MsgPack.rkt"]}
The author of this library
 @hyperlink["https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ"]{reported a performance hit}
 after narrowing some types from @tt{Any} to a more-precise union type for serializable inputs.@note{@hyperlink["https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ" @tt{https://tinyurl.com/5f7mhks2}]}
Tests that formerly passed on the package server timed out after the change.

After changing one bridge module from @|sDeep| to @|sShallow|
(a one-line change),
the time needed to run all tests improved from @${320} seconds to @${204} seconds.
Migrating the rest of the library to @|sShallow| Racket adds only a slight
improvement (down to 202 seconds), which suggests that a mix
is best for MsgPack.

@; can do even better after changing the code:
@;  Deep, no pack casts = 117 seconds
@;  Shallow, no pack casts = 67 seconds
@;  untyped = 24 seconds!!!


@parag{External Data, JSON}
@(let* ((script_name "QA/transient-expressive/json/custom.rkt")
        (s* '(169 157 162 159 162))
        (t* '(3007 2991 2920 3096 3308))
        (t/s (/ (mean t*) (mean s*)))
        (slowdown (if (< 10 t/s) "over 10x" (format "~ax" (rnd t/s)))))
  @elem{
Typed code that deals with data from an external source is often better off
 in @|sShallow| Racket because it validates data lazily.
By contrast, @|sDeep| Racket eagerly traverses a data structure as soon as it
reaches a type boundary.
If the boundary types allow mutable values, then the traversal is even more
 expensive because it creates wrappers as it copies the dataset.
To illustrate the pitfall, the first author wrote a typed script that reads a large dataset of
 apartment data using on off-the-shelf JSON parser and accesses one field
 from each object in the dataset.
@|sDeep| Racket runs the script @|slowdown| slower than @|sShallow| Racket.
})

In principle, @|sDeep| can avoid the slowdown with a custom parser
 that validates data as it reads it.
Indeed, Phil Nguyen has written a @hyperlink["https://github.com/philnguyen/json-type-provider"]{library}
 for JSON that mitigates the overhead of @|sdeep| types.
Such libraries are ideal, but until we have them for the next data exchange
 format (CSV, XML, YAML, ...) @|sShallow| offers a pragmatic solution.

