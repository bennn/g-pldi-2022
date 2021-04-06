#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS Deep + Shallow is better than Deep or Shallow
@;  - saves guarantees, performance, and expressiveness
@;  - TODO is expressiveness incidental?
@;  - maybe add constraints = no new compiler, no new language

@; 2021-04-06 the "disappointing success" pitch doesn't work, it's too focused on performance
@;  can't mix 2 slow semantics to get faster than erasure. No way!

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


@title[#:tag "sec:introduction"]{A Way Forward}

%% what to do, gradual vs migratory

Natural gradual typing offers the strongest guarantees and is clearly the
 way to go.
Adding Natural to an existing language, though, brings huge performance
 problems.
Researchers have addressed these problems by designing new compilers
 and or new languages all together.
Very good, but it misses the migration benefit of gradual typing.
After all if you can adopt a new language why not pick a
 static.
Anyway we still need a fix for teams that cannot switch languages, if
 migratory typing is going to pay off.

Solution-idea comes from related work on weaker gradual semantics.
In particular Transient.

Contributions:
- deep and shallow can coexist
- union brings measurable benefits:
  stronger guarantees for shallow (? after migration ?)
  better performance for deep (after simple lang change)
  more expressiveness (incidental?)

