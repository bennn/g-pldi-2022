#lang scribble/acmart @sigplan @10pt @natbib[#f] @screen

@(require "bib.rkt" (only-in "main.rkt" if-techrpt))

@title{Deep and Shallow Types for Gradual Languages}

@;@(define NEU
@;   @affiliation[
@;     #:institution "PLT @ Northeastern University"
@;     @;#:city "Boston"
@;     @;#:state "Massachusetts"
@;     @;#:postcode "02115"
@;     #:country "USA"
@;   ])

@(define Brown
   @affiliation[
     #:institution "Brown University"
     @;#:city "Providence"
     @;#:state "Rhode Island"
     @;#:postcode "?????"
     #:country "USA"
   ])

@author["Ben Greenman"
        #:email "benjamin.l.greenman@gmail.com"
        #:orcid "0000-0001-7078-9287"
        #:affiliation Brown]

@; -----------------------------------------------------------------------------

@abstract{
Sound gradual types come in many forms and offer varying levels of
soundness.
Two extremes are deep types and shallow types.
Deep types offer compositional guarantees but depend on
expensive higher-order contracts.
Shallow types enforce only local properties, but can be implemented
with first-order checks.
This paper presents a language design that supports both deep and shallow
types to utilize their complementary strengths.

In the mixed language, deep types satisfy a strong complete monitoring guarantee
and shallow types satisfy a first-order notion of type soundness.
The design serves as the blueprint for an implementation
in which programmers can easily switch between deep and shallow
to leverage their distinct advantages.
On the GTP benchmark suite, the median worst-case overhead drops from
several orders of magnitude down to 3x relative to untyped.
Where an exhaustive search is feasible, 40% of all configurations run fastest
with a mix of deep and shallow types.
}

@ccsdesc[#:number 500]{Software and its engineering~Semantics}
@ccsdesc[#:number 100]{Software and its engineering~Constraints}
@ccsdesc[#:number 100]{Software and its engineering~Functional languages}

@keywords{gradual typing, migratory typing, complete monitoring, type-enforcement strategies}

@; -----------------------------------------------------------------------------

@include-section{introduction.scrbl}
@include-section{background.scrbl}
@include-section{model.scrbl}
@include-section{implementation.scrbl}
@include-section{evaluation.scrbl}
@include-section{related.scrbl}
@include-section{future.scrbl}
@include-section{conclusion.scrbl}

@if-techrpt[
 @include-section{appendix.scrbl}]

