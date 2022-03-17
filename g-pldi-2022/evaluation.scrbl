#lang scribble/acmart
@(require "main.rkt" "bib.rkt"
   (only-in racket/math
     exact-round)
   (only-in math/statistics
     mean
     median)
   (only-in gtp-plot/performance-info
     performance-info->num-units)
   (only-in "../data/analyze.rkt"
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

@(define mix-best-D*
   (let ((*bb (box #f)))
     (lambda ()
       (or (unbox *bb)
           (let ((v
                  (find-lowest-3dpath-D*
                    '(forth fsm fsmoo mbta morsecode zombie dungeon
                      jpeg zordoz lnm suffixtree kcfa snake take5))))
             (set-box! *bb v)
             v)))))

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
  for a block of code (@section-ref{sec:evaluation:guarantees}).
  @; Types improve from local spot-checks
  @; to claims that hold throughout the program, including in untyped modules.
}
@item{
  Switching from @|sDeep| to @|sShallow| can remove spurious errors
  from a program (@section-ref{sec:evaluation:expressiveness}).
  @; because @|sshallow| types enforce fewer constraints.
}
@item{
  The combination of @|sDeep| and @|sShallow| improves worst-case
  overheads relative to untyped code (@section-ref{sec:evaluation:performance}).
  @; @|sDeep| Racket adds no overhead among typed modules and
  @; reaps the full benefits of type-directed optimizations.
  @; @|sShallow| Racket avoids the high worst-case overheads of contracts.
}
]


@section[#:tag "sec:evaluation:guarantees"]{Guarantees by @|sDeep|}

By design, @|sdeep| types enforce stronger guarantees than @|sshallow|.
A @|sdeep| type is a behavioral claim that is substantiated by comprehensive
run-time checks.
No matter where a @|sdeep|-typed value ends up, the type constrains its behavior.
A @|sshallow| type is valid only in a limited scope.
If a value escapes to untyped or less-precisely typed code, e.g., via subtyping,
then its original type gets forgotten (@section-ref{sec:background:deep-shallow}).

Prior work suggests that the relative weakness of @|sshallow| types can lead to
confusing situations.
@citet{lgfd-icfp-2021} performed an automated study of debugging in
Typed Racket and found that @|sShallow| blame errors are more likely to
reach a dead end than @|sDeep| blame errors.
@citet{tgpk-dls-2018} conducted a survey using a hypothetical gradual language
and reported that participants found the behaviors allowed by @|sshallow| types
``unexpected'' more often than @|sdeep| types.
Migrating from @|sshallow| to @|sdeep| types may therefore be an effective way of
finding the root issue in a buggy program.


@section[#:tag "sec:evaluation:expressiveness"]{Expressiveness by @|sShallow|}
@; - TODO can we fix occurrence typing? Any -> box? -> set-box! ???
@;    ditto for objects, avoid the cast, is it worth changing the typechecker?
@; - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow
@; - higher-order / any errors, gone
@; - indexof ... weird result, gone

@|sShallow| Racket can express a variety of useful programs that @|sDeep| Racket
rejects at run-time.
At first glance, the existence of such programs is surprising because
the theory suggests that the @|sdeep| semantics is more ``correct'' (@section-ref{sec:model}).
It turns out that @|sdeep| types can be overly restrictive; in such programs,
delayed @|sshallow| checks work better in practice.
In other programs, the gap between @|sDeep| and @|sShallow| Racket is due
to implementation issues.
Refer to the @appendixref{appendix:expressiveness} for motivating example programs
that were submitted by Typed Racket users.
@; Language designers should be aware of these issues when building a new
@; @|sdeep| semantics.


@subsection[#:tag "sec:evaluation:expr:any"]{Less-strict Top Type}

Statically, the top type is a supertype of every other type.
Programmers often use this type as a convenient placeholder to avoid
committing to a more-specific type.
When enforced as a @|sdeep| type, however, the top type has a strict
semantics that prevents clients from inspecting top-wrapped values@~cite{fb-tr-2006}.
For example, if @|sdeep| code exports a function using a top type,
then non-@|sdeep| clients cannot invoke the function.

The @|sshallow| top type imposes no such restrictions.
@; on its clients.
Untyped code may invoke a @|sshallow| function exported via the top type,
and may even write to a top-typed array.
These freedoms can be useful, and do not undermine the weak @|sshallow|
soundness guarantee.


@subsection[#:tag "sec:evaluation:expr:wrap"]{No Missing Wrappers}

Mutable values that can appear in @|sdeep| code need tailored
wrappers to monitor their interactions with non-@|sdeep| clients.
These wrappers are difficult to implement because they often require support from the
run-time system@~cite{stff-oopsla-2012}.
@(let* ((missing-wrapper* '(
          "(Async-Channel T)" "(Custodian-Box T)" "(C-Mark-Key T)" "(Evt T)"
          "(Ephemeron T)" "(Future T)" "(MPair T T')" "(MList T)"
          "(Prompt-Tag T T')" "(Syntax T)" "(Thread-Cell T)" "(Weak-Box T)"))
        (num-missing (length missing-wrapper*)))
  @elem{
Unsurprisingly, some infrequently-used types in @|sDeep| Racket
lack wrappers (@~a[num-missing] in total).
})

By contrast, a @|sshallow| language avoids the question of how
to implement wrappers.
@|sShallow| types need only first-order checks, which require far
less engineering.


@subsection[#:tag "sec:evaluation:expr:uniform"]{Uniform Behavior}

Although the purpose of @|sdeep| wrappers is to reject type-incorrect
operations without otherwise changing behaviors,
certain wrappers in @|sDeep| Racket do cause subtle changes.
The most problematic ones are the wrappers for polymorphic types.
@|sDeep| Racket enforces types such as @tt{(All (A) (-> A A))} with
a function contract that seals inputs and unseals outputs@~cite{gmfk-dls-2007}.
The seals change the outcome of basic operations.

@|sShallow| Racket avoids all such changes in behavior,
 including the well-known object identity issues@~cite{stff-oopsla-2012,kt-icfp-2015,vksb-dls-2014,vm-ecoop-2013},
 because the @|stransient| semantics does not use wrappers.


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
toggling between @|sdeep| and @|sshallow| avoids
pathological cases.
Mixing @|sdeep| and @|sshallow| modules can further improve performance,
up to 2x faster relative to untyped code.

All data in this section was collected on a single-user Linux box
with @~a[NSA-num-cores] physical @~a[NSA-core-name] @~a[NSA-core-speed] cores and @~a[NSA-RAM] RAM.
The machine ran Racket v7.8.0.5
(@github-commit["racket" "racket" "7c903871bd8cb4bd32ed7188c180b5124f9bc201"]).
and a pre-release of Shallow Racket (@github-commit["bennn" "typed-racket" "c074c9333e467cb7cd2058511ac63a1d51b4948e"])
that extends Typed Racket v1.12.
@; no need for a TR commit, sends the wrong message --- reproductions need not check that out
Each data point is the result of running one program configuration nine times in a row
 and averaging the speed of the final eight runs.


@subsection[#:tag "sec:evaluation:perf:together"]{Deep and Shallow Combined}
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
        (fsm-mixed (- fsm-num-configs fsm-non-mixed))
        [path-3d-D 1.2]
        [path-3d
         (for/list ((bmx (in-list (mix-best-D*)))
                    #:when (and (cdr bmx) (<= (cdr bmx) path-3d-D)))
           (bm (car bmx)))]
       )
@list[
@elem{
Mixing @|sdeep| and @|sshallow| types within one program configuration can improve its
performance.
Such configurations are common in the GTP benchmarks.
Out of the @${2^N} configurations in @integer->word[num-DDD] of the smaller
benchmarks, a median of @$[@~a[DDD-median] "\\%"] run fastest
with a mix of @|sdeep| and @|sshallow| types (@figure-ref{fig:both:3way}).
@; These results are especially encouraging because @${N\!+\!1} configurations in
@; each benchmark cannot mix @|sdeep| and @|sshallow| because they contain fewer
@; than @${2} typed modules.
@; In @bm{fsm}, for example, there are @integer->word[fsm-num-configs] mixed-typed configurations.
@; @Integer->word[fsm-non-mixed] of these have at most one typed module.
@; Of the remaining @~a[fsm-mixed] configurations, over half run fastest with a
@; combination of @|sdeep| and @|sshallow| types.
These mixtures also increase the number of @emph{@ddeliverable{D} migration
paths} (defined in @secref{sec:evaluation:perf:path}).
All paths in @oxfordize[path-3d] are @ddeliverable[path-3d-D]
when configurations can mix @|sdeep| and @|sshallow| types.

These encouraging numbers are the result, however, of an exhaustive search through
@${3^N} configurations.
The following three subsections therefore investigate whether Deep and Shallow
mixtures can offer benefits without an infeasibly-large search.
}
@figure[
  "fig:both:3way"
  ;; TODO nicer symbol than + for the mix? ... || is great for either-or ... \cup vs \uplus ???
  @elem{
    Percent of configurations that run fastest with a mix of @|sDeep| and @|sShallow| modules.
  }
  (render-3d-table-y DDD)]
])


@subsection[#:tag "sec:evaluation:perf:case"]{Case Studies}

To test whether fast-running configurations can be found without a search,
we manually explored @|sdeep| and @|sshallow| combinations in the following
programs:

@parag{MsgPack}
@hyperlink["https://gitlab.com/HiPhish/MsgPack.rkt"]{MsgPack} is a Typed Racket
 library that converts Racket values into serialized
 @hyperlink["http://msgpack.org/"]{MessagePack} data.@note{@shorturl["https://" "gitlab.com/HiPhish/MsgPack.rkt"]}
The author of this library
 @hyperlink["https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ"]{reported poor performance}
 due to @|sdeep| type boundaries.
Changing one bridge module from @|sdeep| to @|sshallow| types
(a one-line change), however,
reduces the time needed to run all tests from @${320} seconds to @${204} seconds
(@${40\%} speedup).

@; Migrating the rest of the library to @|sShallow| Racket added only a slight
@;  improvement (down to 202 seconds).
@; Can do even better after changing the code:
@;  Deep, no pack casts = 117 seconds
@;  Shallow, no pack casts = 67 seconds
@;  untyped = 24 seconds!!!

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
The @bm{synth} GTP benchmark is based on @hyperlink[synth-url]{an untyped program}
that interacts with a @|sdeep|-typed math library to synthesize music.@note{@shorturl["https://" synth-short-url]}
This untyped program runs @~a[deep-delta]x slower than a @|sdeep|-typed
version because of the library boundary.
When the library uses @|sshallow| types instead, the gap between an
untyped and @|sdeep|-typed client improves to @~a[shallow-delta]x.
@; This fast untyped configuration is roughly @~a[ds-fast]x slower than the fast
@; @|sdeep|--@|sdeep| configuration, but the worst-case is @~a[ds-slow]x
@; faster (@~a[ds-slow-sec] seconds) than before.
@; Overall, a @|sShallow| version of the math library is a better tradeoff.
})


@; @parag{External Data, JSON}
@; @(let* ((script_name "QA/transient-expressive/json/custom.rkt")
@;         (s* '(169 157 162 159 162))
@;         (t* '(3007 2991 2920 3096 3308))
@;         (t/s (/ (mean t*) (mean s*)))
@;         (slowdown (if (< 10 t/s) "over 10x" (format "~ax" (rnd t/s)))))
@;   @elem{
@; Typed code that deals with data from an external source is often better off
@;  in @|sShallow| Racket because it validates data lazily.
@; By contrast, @|sDeep| Racket eagerly traverses a data structure as soon as it
@; reaches a type boundary.@note{
@;   In principle, @|sDeep| can avoid the slowdown with a custom parser
@;    that validates data as it reads it.
@;   Indeed, Phil Nguyen has written a @hyperlink["https://github.com/philnguyen/json-type-provider"]{library}
@;    for JSON that mitigates the overhead of @|sdeep| types.
@;   Such libraries are ideal, but until we have them for every data exchange
@;    format (MessagePack, CSV, YAML, and so on) @|sShallow| offers a pragmatic solution.
@; }
@; If the boundary types allow mutable values, then the traversal is even more
@;  expensive because it creates wrappers as it copies the dataset.
@; To illustrate the pitfall, we wrote a typed script that reads a large dataset of
@;  apartment data using on off-the-shelf JSON parser and accesses one field
@;  from each object in the dataset.
@; @|sDeep| Racket runs the script @|slowdown| slower than @|sShallow| Racket.
@; })


@subsection[#:tag "sec:evaluation:perf:either-or"]{Deep or Shallow, Worst-Case}

@(let* ((WT (get-mixed-worst-table SHALLOW-CURRENT-BENCHMARK*))
        (has-typed-lib-or-both* '(acquire fsm fsmoo gregor kcfa lnm mbta quadT quadU snake suffixtree synth take5 tetris zombie zordoz))
        (num-typed-lib (length has-typed-lib-or-both*))
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
Both @|sdeep| and @|sshallow| implementations have known bottlenecks.
With @|sdeep| types, high-traffic boundaries can lead to huge
costs@~cite{htf-hosc-2010,tfgnvf-popl-2016,gtnffvf-jfp-2019}.
With @|sshallow| types, every line of typed code contributes a small
cost@~cite{vss-popl-2017,gm-pepm-2018}.

By switching between @|sDeep| and @|sShallow| a programmer can often, however,
avoid the worst-cases of each.
@Figure-ref{fig:evaluation:mixed-worst-table} quantifies the benefits of
this either-or strategy on the GTP benchmarks.
The first column shows that, as expected, @|sdeep| types may have
enormous costs.
The second column shows that the worst configurations for @|sShallow| Racket are
far less severe.
The third column shows, however, that toggling between @|sDeep| and @|sShallow|
Racket can do even better.
Numbers in this third column are typeset in @bold{bold} if they
are the best (lowest) in their row.
The @tt{sieve} and @tt{tetris} benchmarks are notable successes.
The @tt{zombie} benchmark is the worst.
@|sDeep| Racket pays a huge cost in @tt{zombie} because functions
repeatedly cross its module boundaries.
@|sShallow| Racket also pays a relatively high cost because @tt{zombie}
uses functions to simulate message-passing objects, and therefore contains
a large number of elimination forms that incur shape checks.

@bold{Note}: This either-or strategy is possible in general only because @|sDeep|
and @|sShallow| can interoperate.
Indeed, most of the benchmarks (@~a[num-typed-lib]/21) rely on @|sdeep|-typed
code that lives outside their @${N} core migratable modules.
Without interoperability, the outside code would require changes that are
unrealistic to make in practice.
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
        )
@list[
@figure[
  "fig:both:mixed-path"
  ;; TODO barchart? yes yes yes
  @elem{
    Percent of @ddeliverable[D] migration paths for @|sDeep| alone, @|sShallow| alone, and an either-or mix.
  }
  @render-mixed-path-table[PT]
]
@elem{
The complementary strengths of @|sDeep| and @|sShallow| Racket can help
programmers avoid bottlenecks as they migrate an untyped codebase to a
typed configuration.
Consider the set of all migration paths, each of which begins at the
untyped configuration and adds types to one module at a time until
reaching the fully-typed configuration.
A path is @ddeliverable{D} if all of its configurations
run at most @${D} times slower than the untyped configuration.

@Figure-ref{fig:both:mixed-path} counts the proportion of @ddeliverable{3}
paths out of all @${N!} migration paths in a subset of the GTP benchmarks.
Larger benchmarks are omitted.
The first column counts paths in @|sDeep| Racket,
the second column counts paths in @|sShallow| Racket, and
the third column counts paths using @|sDeep| or @|sShallow| at each point.
With @|sDeep| alone, all paths in @integer->word[(length deep-dead*)]
benchmarks reach a bottleneck that exceeds the @~a[D]x limit.
With @|sShallow| alone, all paths in @integer->word[(length shallow-dead*)] benchmarks
 exceed the limit as well---often near the end of the migration path.
With the either-or mix, only @integer->word[(length mix-dead*)] benchmark (@bm[(car mix-dead*)])
 has zero @ddeliverable[D] paths.
}])


