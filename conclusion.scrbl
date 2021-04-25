#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; bring in analogy to GT? typed + untyped ... weak-typed + strong-typed ?

@; future, better ways to coexist?

@; ps Greenberg is a 2nd impasse, stat-first vs dyn-first ... worth mentioning?

@title[#:tag "sec:conclusion"]{Conclusion}

A downside of the combination is that @|snatural| and @|stransient| cannot
 easily share the results of their type checks.
The reason is simple: @|stransient| as-is lacks a way of learning from past checks.
@Sectionref{sec:future:nonopt} explains the synergy challenge in terms of the
 model and outlines implementation techniques that may get around the issue.


The End

@exact|{\bibliography{bg}}|
