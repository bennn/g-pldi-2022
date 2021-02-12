#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt")

@; THESIS the TR compiler can support shallow
@; - outside-facing API : new lang for users, easy switch
@; - inside-facing API : reuse ~80% of compiler
@; - note typed libraries still deep
@; - note awkardness at typed-untyped utils
@;
@; - for technical details, point to PR or dissertation

@title[#:tag "sec:implementation"]{Implementation}

The Typed Racket compiler can incorporate Shallow types.

