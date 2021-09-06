POPL 2022 Paper #439 Reviews and Comments
===========================================================================
Paper #439 Deep and Shallow Types for Gradual Languages


Review #439A
===========================================================================

Overall merit
-------------
C. Weak Reject

Reviewer expertise
------------------
X. Expert

Paper summary
-------------
There are two main techniques to enforce type-based invariants in
gradual typing: the Natural strategy, which uses wrappers to enforce
function types, and the Transient strategy, which uses only
first-order checks.  This paper investigates a language that allows
mixing the two strategies (for the module-level, coerce-grained
gradual typing).

The first part of the paper investigates the theoretical aspects of
such a language.  Following Greenman et al.'s work, the paper sets up
a three-way mix-typed language, compilation into an evaluation
language, in which run-time checks are made explicit, and operational
semantics of the evaluation language.  Type soundness and complete
monitoring properties are stated (and their proof sketches are given).

The second part is concerned about more practical issues.  In Section
4, issues of implementating the mix typed language for Racket are
discussed.  Section 5 gives a general qualitative comparison the two
strategies and performance evaluation to measure how programs could
benefit from mixing the two strategies.

Strengths
---------
* The topic is interesting.  In previous work, one can mix typed and
untyped modules with a fixed implementation scheme but the paper
enables mixing typed modules with different implementation schemes and
run-time guarantees, refining the spectrum of gradual typing.

* The writing sometimes feels a bit too "Racket-oriented" but the
overall quality of presentation is good: examples are practically
motivated and helpful.
  
* The proposed mechanism is implemented as an extension of (Typed)
  Racket.

* Experiments using the GTP benchmark suite are conducted to show that
how programs could benfit from mixing the Natural and Transient
strategies.

Weaknesses
----------
* The statement of type soundness is weaker than what one would expect.

* The experimental settings are not very clearly described.

Comments for author
-------------------
Theorem 3.2 looks too weak.  In particular, it doesn't exclude
run-time errors even for deep-typed programs.  Of course, a deep-typed
program may contain untyped or shallow-typed expressions inside and it
is not possible to exclude errors but I'd expect that TagErr shouldn't
occur in a deep-typed conext.  If one examines the type system, it
wouldn't be difficult to see that app v0 v1 in a deep-typed context
won't result in a TagErr but the statement of Theorem 3.2 doesn't
capture it.

Performance evaluation in Section 5.3 is interesting but it's not
always to clear how numbers are calculated.  I think the paper should
give a minimal discussion about the experimental setting, like what
configurations are, what is the baseline to calculate overheads.

The discussion on interactions with Racket specifics is interesting
but Section 4 is not easy to follow for those who have only
superficial knowledge about Racket.

# Minor comments:

Periods are missing at several places like L74, L141,

Fig.8: Should $\sigma_0$ in the rule for $unop$ Pair?

L606: 4 vs. 42.

L705: $\ell_2$ should be $\ell_1$.

L706 and L707: Why is the context label $last(\bar \ell_0)$, rather than the first one?

L890: On the one hand, you seems to say that it is problemaic that
"there is no guarantee that typed-f and untyped-f have the same
behavior".  On the other hand, you say "the
define-typed/untyped-identifier" form would benefit from a third
argument, which sounds more problematic to me.

L1001: "well know"

L1064: "switching between the two often performance bottlenecks" misses a verb?

L1298: There are two bib entries for Wrigstand et al. 2010.



Review #439B
===========================================================================

Overall merit
-------------
C. Weak Reject

Reviewer expertise
------------------
Z. No familiarity

Paper summary
-------------

This paper incorporates both Natural and Transient semantics in the 
same language, exploring the theoretical implications and showing
promising results in practice when (expertly) mixing the two approaches.

Strengths
---------
- The technical development fully encompasses both forms of gradual type 
enforcement, and has the potential to be what "Call-by-push-value" is for
exploring CBV/CBN but for gradual typing.
- The implementation appears to be useful in practice, in that experts 
can maximize its potential by mixing shallow and deep types in programs.

Weaknesses
----------
Evaluation+Motivation w.r.t Corpse Reviver [Moy et al., POPL 2021].

While the intuition behind deep contracts leading to higher overheads rings
true, (most of) the benchmarks chosen for the evaluation can be targeted by a 
simple static analysis to almost completely eliminate any overhead. While 
the approach presented here is seemingly more useful than the two approaches
it encompasses, it is really lacking a comparison and a discussion with 
the Moy et al alternative which is seemingly even better in the benchmarks 
considered.

Comments for author
-------------------
Minor:
l 29. Contracts in Natural -> Natural contracts/Contracts in Natural style? 
- Also appears in the rest. The constant use of Natural/Transient as both 
an adjective and a noun (sometimes within the same/adjacent sentences) threw
me off.


l 236: It's not just the subsumption that makes it not syntax-directed, right? 
  Isn't the fact that a module can have multiple types even worse?

Questions for the response period
---------------------------------
Questions: 
- Am I missing something w.r.t to Moy et al? While yours and their approach 
are relatively orthogonal, theirs seems more effective if the goal is to 
preserve soundness guarantees while improving efficiency. 
- How much tinkering was necessary to achieve the best-case mixing of shallow
and deep types in practice? Is it something that required deep knowledge of 
the underlying implementation or is it something that a Racket newcomer would
be able to do?



Review #439C
===========================================================================

Overall merit
-------------
B. Weak Accept

Reviewer expertise
------------------
X. Expert

Paper summary
-------------
In gradual typing, type-based guarantees come with a run-time cost.
To mitigate this, researchers have experimented with a variety of
enforcement styles and strength of guarantees.  This paper develops a
language model that combines two styles from the literature, Natural
and Transient, allowing a programmer to blend both to suit their
needs.

Strengths
---------
Overall well written

Clear presentation of the language model

Weaknesses
----------
The rationale for some of the ownership rules are not immediately
obvious.  More explanation is needed if others are to build on this
work.

The exposition of the implementation was hard to follow, especially
how it connects to the model.  I found it hard to understand the
discussion of macros and how they interact with the two type
enforcement schemes.

Comments for author
-------------------
line 9: "known strategy that provides *strong* guarantees and
performance"  as opposed to any guarantees whatsoever

line 89:  is deep vs. shallow synonymous with natural vs. transient?
It would be good to clarify the terminology.

line 99:  the discussion of the middle typed interface confused me at
first, because the text function is used in a first-order manner, but
the transient behavior is the result of text only being checked as a
function.  A little more explanation of this here would help.

line 158:  natural numbers n are missing

line 184:  what counts as a top-level program in this model?

line 195:  It was unclear here what the point of the modules were
since either kind of code could have either kind of module wrapper.  A
little more explanation of intent please.

line 223:  It would help to read this if the module typing rules were
visually typeset similarly for easier comparison.

line 234: Say that the annotation on a lambda determines what kind of
expression it is.

line 238:  within a deep expression via a module *U* boundary

line 239: It appears that a module label is consistent with what is on
the inside.  Worth saying.

line 286:  These three typing judgments were rather mystifying at
first.  Some telegraphing to how they are used later in the metatheory
would help, since this is an unusual setup.

line 334: This discussion suggests that the model presented here is
tailored specifically to a "dynamic-first" runtime (like Racket).
What would change for a system that does not assume dynamic runtime
checks by default, but instead "completes" dynamic programs?

line 393:  Why is this figure missing a deep lambda?  Is that
intentional?

line 419:  FYI the appendix gets the second rule on this line wrong
(the lambda with a scan in the conclusion)

line 493:  Why is only the outermost label kept here?

line 495 and 501:  Why are unops and binops treated differently?  What
is the rationale for that?

line 520 and 525 :  Why does a ScanErr and WrapErr only have one owner?

line 532:  why can vec{l0} go away here?

line 563:  "the labels on an expression describe the surface modules
that are responsible for the behavior of the expression."  What
precisely does it mean for a module to be responsible for a behavior?

line 611:  Boundary expressions and _guarded_ values are...

line 632:  I don't understand the connection between this and, say,
Lemma A.1.  Please clarify.

line 698: "note that the binary operators are not elimination forms in
the model"  I wish this were said outside of the confines of the
proof.

line 785: "appropriate"

line 790:  why is dead-code elimination unsound?

line 799: "compiled"

line 817:  I don't understand this discussion.  Line 778 says that
type checking happens _after_ macro expansion.  So what's going on
here?  Are macros type checked?  Is elaboration not like the model?

line 842:  I still found this section confusing, possibly because of
being confused in the prior note.

line 1064:  "between the two often ___ performance bottlenecks"  a
word missing

line 1107:  "runs better _than_ either alone."

line 1202: "switch back to deep _to_ maximize performance"
