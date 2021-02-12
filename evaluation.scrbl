#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS Deep + Shallow > Deep | Shallow
@; - sections for guarantees, performance, and expressiveness
@; - is expressiveness incidental?
@;   * Any => Procedure => (f x) ... may require static typing changes to really improve
@; - focus on worst case for performance

@title[#:tag "sec:evaluation"]{Evaluation}

A mix of Deep and Shallow types is better than either alone.

