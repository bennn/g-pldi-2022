#lang scribble/acmart @acmsmall @10pt @review @anonymous

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

@author["Matthias Felleisen"
        #:email "matthias@ccs.neu.edu"
        #:affiliation NEU]


@; -----------------------------------------------------------------------------

@keywords{gradual typing, migratory typing, complete monitoring}
@; TODO is there really no new vocabulary that we earned along the way???!

@; -----------------------------------------------------------------------------

@abstract{
Research on sound gradual typing has identified several techniques for mixing
typed and untyped code.
These strategies have complementary strengths in terms of type system guarantees
and performance.
Two extremes are the natural and transient strategies.
Natural offers strong @emph{deep} types but depends on higher-order contracts.
Transient offers weak @emph{shallow} types, but can be implemented with
unobtrusive first-order checks.
There is no known strategy that provides guarantees and performance
without placing restrictions on untyped code.

@; Programmers would benefit from the ability to mix and match different
@; type-enforcement strategies within the same program.

This paper presents a language design that supports both deep and shallow
types.
In the mixed language, deep types satisfy a strong complete monitoring guarantee
and shallow types satisfy a first-order notion of type soundness.
The design serves as the blueprint for a Typed Racket implementation
in which programmers can easily switch between deep and shallow types
to leverage their distinct advantages.
On programs from the GTP benchmark suite, 40% of all configurations run fastest
with a mix of deep and shallow types.
}

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

