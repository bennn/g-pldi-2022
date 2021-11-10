#lang scribble/acmart @sigplan @10pt @review @anonymous

@(require "bib.rkt" (only-in "main.rkt" if-techrpt))

@title{Deep and Shallow Types for Gradual Languages}

@(define NEU
   @affiliation[
     #:institution "PLT @ Northeastern University"
     @;#:city "Boston"
     @;#:state "Massachusetts"
     @;#:postcode "02115"
     #:country "USA"
   ])

@(define NWU
   @affiliation[
     #:institution "PLT @ Northwestern University"
     @;#:city "Evanston"
     @;#:state "Illinois"
     @;#:postcode "xyzuv"
     #:country "USA"
   ])

@author["Ben Greenman"
        #:email "benjaminlgreenman@gmail.com"
        #:orcid "0000-0001-7078-9287"
        #:affiliation NEU]

@; @author["Matthias Felleisen"
@;         #:email "matthias@ccs.neu.edu"
@;         #:affiliation NEU]


@; -----------------------------------------------------------------------------

@keywords{gradual typing, migratory typing, complete monitoring}
@; TODO is there really no new vocabulary that we earned along the way???!

@; TODO CCSXML

@; -----------------------------------------------------------------------------

@abstract{
Sound gradual types come in many forms and offer varying levels of type
soundness.
Two extremes are @emph{deep} types and @emph{shallow} types.
Deep types offer compositional guarantees but depend on
expensive higher-order contracts.
Shallow types offer only a soundness guarantee, but can be implemented
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

@; Research on sound gradual typing has identified several techniques for mixing
@; typed and untyped code.
@; These strategies have complementary strengths in terms of type system guarantees
@; and performance.
@; Two compelling extremes are the Natural and Transient strategies.
@; Natural offers strong @emph{deep} types but depends on expensive higher-order contracts.
@; Transient offers weak @emph{shallow} types, but can be implemented with
@; unobtrusive first-order checks.

@; Programmers would benefit from the ability to mix and match different
@; type-enforcement strategies within the same program.

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
 @include-section{blank.scrbl}
 @include-section{appendix.scrbl}]

