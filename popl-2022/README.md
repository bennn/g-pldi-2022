deep and shallow
===

Goal: move chapter 6 of Ben G. dissertation to a paper

Target: POPL 2022
- deadline 2021-07-08
- cfp <https://popl22.sigplan.org/track/POPL-2022-popl-research-papers#POPL-2022-Call-for-Papers>
- 25 pages 10pt font


MF outline:

> I thought about your unpublished research contribution in your thesis and came up with a thesis approach. 
>
> Instead of tackling it from a “performance” angle, which may get attacked by
> “performance knights” on a PC, you could try the idea of “let’s generalize
> along a different dimension”.  And then we can ask the question “are there
> benefits” and you can explain both “more programs run” and “you don’t die when
> you migrate”.  
>
> I sketched it out via a 2-slide diagram. The PDF is attached. You could try to
> write an introduction like I did for the ICFP experience paper. Your best bet
> is to work toward POPL in July. But get started soon. Time will fly once you
> work with Shriram. 


### 2021-06-27 talk idea

- you've heard of GT / MT / .... cottage industry
  - simple idea, mixed-typed
  - disagree on details
  - my ground rules = migratory, non-dyn (general baseline)
- among systems that meet ground rules, lots of variety,
  particular about _extent_ of types, for lack of better word
  - example program, Deep vs Shallow, keep it simple
  - shallow types = sound. And soundless says nothing in untyped code.
  - deep types = type obligations interpose all behaviors. Characterized by CM.
    ... closer to my intuition
- why two styles? complementary strengths
  - stay vague: S, good mix ; D, good typed
- esp. when comes to implementation, Natural vs Transient
  - Natural = contract at boundary
  - Transient = first-order everywhere

- in this paper, show htat D + SN can interoperate and implement it
  - key idea D--S--U triangle,
    slowly label the edges,
    maybe begin with 6 edges, then simplify?
    notation for sketch / imagine vs. finalize edge
  - properties (on same finalize slide)
    1 2 3 soundness
    monitoring property, deep single-owner

- implemented by extending TR compiler
  - some excitement at DS interface, see paper (? save to end)
- benefits, from dissertation = guarantees + expr + perf ... or perf + expr
  - maybe reuse the heavy CM example
  - show 3way numbers

- in conclusion, analogy to GT
  - got started on this road by putting T/U knob in programmers hands
    turns out tradeoffs from types call for another knob
  - survey, recall "worst of both"
  - the end ... if recorded try to stop sooner don't worry about questions


### 2021-02-10 outline + intro

- from 2 to 3 and why
- background and constraints
- why TR
- why Natural, why Transient ... which semantics cannot fit? migratory
  - behavioral, cm
  - proxy free
- model and proofs
- implementation, compiler reuse (brief), boundary challenges
  + unclear what's of general interest here
- benefits catalog
- taking stock
  + can be done
  + surprising benefits, esp. expressiveness, call for user studies
  + surprising lack of N T synergy
    . look for new compromise semantics (seems unlikely)
    . find better combo
  + ... any more?


