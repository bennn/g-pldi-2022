#### General Remark

Note that the idea of mixing type enforcement strategies is a **novel** idea
for gradually-typed languages.

- Until recently, researchers hardly acknowledged that strategies with complementary
  strengths existed.
  + Papers that introduced strategies focused on soundness as the main formal
    criteria, and therefore lacked a firm comparison to other sound strategies.
    E.g., [Vitousek et al. POPL 2017, Castagna et al. ICFP 2017].
  + Even the effort to precisely characterize gradual typing with _gradual
    guarantees_ was oblivious to changes in strategy [Siek et al. SNAPL 2015].
    Natural, Transient, and others (e.g. [Muehlboeck et al. OOPSLA 2017])
    satisfy all the guarantees.

- Consequently, there were only two research questions in this space:
  1. Can we develop a "better" strategy than all the rest? (intentionally vague)
  2. Can we improve an existing strategy?

This paper opens a third, orthogonal question (Can we combine existing
strategies?) and kicks off the search with one useful combination.


#### Review #439A

> Comments for author
> -------------------
> Theorem 3.2 looks too weak.  In particular, it doesn't exclude
> run-time errors even for deep-typed programs.  Of course, a deep-typed
> program may contain untyped or shallow-typed expressions inside and it
> is not possible to exclude errors but I'd expect that TagErr shouldn't
> occur in a deep-typed conext.  If one examines the type system, it
> wouldn't be difficult to see that app v0 v1 in a deep-typed context
> won't result in a TagErr but the statement of Theorem 3.2 doesn't
> capture it.

Whether expected or not, type soundness (theorem 3.2) is as strong as it can
be. Your point about untyped expressions inside deep-typed code correctly
explains why.

We can discuss this point in the revision. We can also present the progress
lemmas for deep and shallow code, which do exclude TagErr.


> Performance evaluation in Section 5.3 is interesting but it's not
> always to clear how numbers are calculated.  I think the paper should
> give a minimal discussion about the experimental setting, like what
> configurations are, what is the baseline to calculate overheads.

Yes, the performance section needs to give more details:

- A configuration is a partially-typed version of a program. There are
  2^N configurations for a program with N modules. These configurations
  include fully-untyped and fully-typed versions of the program.

- Overheads are relative to the fully-untyped configuration because it
  avoids the costs of gradual typing.


> The discussion on interactions with Racket specifics is interesting
> but Section 4 is not easy to follow for those who have only
> superficial knowledge about Racket.

Understood. We can revise this section to better explain the general lessons
for language designers, e.g. that macros and type-boundary APIs introduce
side channels.


#### Review #439B

> Questions:
> - Am I missing something w.r.t to Moy et al? While yours and their approach
> are relatively orthogonal, theirs seems more effective if the goal is to
> preserve soundness guarantees while improving efficiency.

Moy et al's approach comes with a caveat about benchmark selection. They do
extremely well on many of the GTP benchmarks because all the untyped code is
written in a well-typed manner and uses little state. Less-disciplined untyped
code can easily thwart the the verifier. Object-oriented code is too complex
for the verifier at this time. See their sections 5.1 and 7 for a discussion.


> - How much tinkering was necessary to achieve the best-case mixing of shallow
> and deep types in practice? Is it something that required deep knowledge of
> the underlying implementation or is it something that a Racket newcomer would
> be able to do?

Figure 20 (l.1030) suggests that good performance is close at hand: either
the fully-shallow or the fully-deep program should run well.

For the revision, we can report the gap between these _either-or_ mixes and
the _best-possible_ mixes that figure 22 (l.1128) refers to. The gaps are
small. The biggest is in zombie, which goes from 31x fully-shallow to 29x
in one mixed shallow/deep configuration. The second-biggest is in fsm and goes
from 1.9x to 0.6x.

In the case studies (section 5.3.3), we found good mixes by studying the
organization of the codebase. The most important detail to learn is whether
a type describes higher-order data. A newcomer could learn this detail.


#### Review #439C

> Weaknesses
> ----------
> The rationale for some of the ownership rules are not immediately
> obvious.  More explanation is needed if others are to build on this
> work.

For the revision, we can bring appendix A.2 into the paper and use it
as a starting point for more explanation of the ownership rules.


> The exposition of the implementation was hard to follow, especially
> how it connects to the model.  I found it hard to understand the
> discussion of macros and how they interact with the two type
> enforcement schemes.

Yes, this section needs another pass to serve a wider audience.
