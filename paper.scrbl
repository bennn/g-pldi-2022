#lang scribble/acmart @acmsmall @10pt @review @anonymous

@(require "bib.rkt" (for-syntax racket/base))

@(define-for-syntax TECHRPT #true)
@(define-syntax (if-techrpt stx)
    (if TECHRPT
      (syntax-case stx ()
       [(_ x ...) #'(begin x ...)])
      #'(void)))

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

@abstract{TBA}

@; -----------------------------------------------------------------------------

@include-section{introduction.scrbl}
@include-section{background.scrbl}
@include-section{model.scrbl}
@include-section{implementation.scrbl}
@include-section{evaluation.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]

@;@if-techrpt[
@; @include-section{blank.scrbl}
@; @include-section{appendix.scrbl}]

