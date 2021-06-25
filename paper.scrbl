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

@keywords{gradual typing, migratory typing}
@; any more?

@; -----------------------------------------------------------------------------

@abstract{
Research on sound gradual typing has identified several compelling strategies
for mixing typed and untyped code.
These strategies have complementary strengths.
Two extremes are the natural and transient strategies, which offer deep
and shallow type guarantees.
Natural has strong types but depends on higher-order contracts.
Transient promises only weak types, but does so with rudimentary first-order checks.

Programmers would benefit from the ability to mix and match different
type-enforcement strategies within the same program.
To this end, we present a language design that supports both deep and shallow
types by integrating the natural and transient strategies.
The design is validated by both a theoretical analysis and an implementation
for Typed Racket.
Shallow types can express more programs, deep types provide stronger
type guarantees and debugging help, and the combination of
deep and shallow frequently out-performs either one alone on the GTP
benchmark suite.
}

@; -----------------------------------------------------------------------------

@include-section{introduction.scrbl}
@include-section{background.scrbl}
@include-section{model.scrbl}
@include-section{implementation.scrbl}
@include-section{evaluation.scrbl}
@include-section{related.scrbl}
@; @include-section{future.scrbl}
@include-section{conclusion.scrbl}

@if-techrpt[
 @include-section{blank.scrbl}
 @include-section{appendix.scrbl}]

