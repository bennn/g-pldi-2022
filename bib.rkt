#lang at-exp racket/base

;; A scriblib/autobib bibliography,
;;  based off Sam Tobin-Hochstadt's gradual-typing-bib:
;;  <https://github.com/samth/gradual-typing-bib>
;;
;; To add a new entry, define a new `make-bib` at the bottom of this file.
;; Look to the previous definitions for examples.

(provide (all-defined-out))

(require racket/format
         (only-in scribble/base elem italic)
         (except-in scriblib/autobib proceedings-location))

(define-cite ~cite citet generate-bibliography) ;  #:style number-style

;; shortens author names
(abbreviate-given-names #f)

(define (to-string v)
  (format "~a" v))

(define (proceedings-location
         location
         #:pages pages
         #:series [series #f]
         #:volume [volume #f])
  (let* ([in (if (eq? pages 'to-appear) "To appear in" "")]
         [s @elem{@|in| @italic{@elem{@to-string[location]}}}]
         [s (if series
                @elem{@|s|, @to-string[series]}
                s)]
         [s (if volume
                @elem{@|s| volume @to-string[volume]}
                s)]
         [s (if (pair? pages)
                @elem{@|s|, pp. @(to-string (car pages))--@(to-string (cadr pages))}
                s)])
    s))

;; ----------------------------------------

;; In a submodule so that it doesn't get exported automatically by
;; the outer module's `(all-defined-out)`
(module util racket/base
  (require racket/format)
  (provide (all-defined-out))

  (define short? #t)
  (define-syntax define/short
    (syntax-rules ()
      [(_ i e e*) (define i (if short? e e*))]
      [(_ i e) (define i e)]))

  (define IEEE "IEEE ")
  (define ACM "")
  (define International "International ")
  (define Conference "Conference ")
  (define Workshop "Workshop ")
  (define Journal "Journal ")
  (define Symposium "Symposium ")
  (define Transactions "Transactions ")

  (define/short aosd "AOSD" (string-append International Conference "on Aspect-Oriented Software Development"))
  (define/short asplas "APLAS" (string-append "Asian " Symposium "Programming Languages and Systems"))
  (define/short asplos "ASPLOS" (string-append International Conference "on Architectural Support for Programming Languages and Operating Systems"))
  (define/short cc "CC" (string-append International Conference "on Compiler Construction"))
  (define/short dls "DLS" "Dynamic Languages Symposium")
  (define/short dsl "DSL" (string-append ACM Conference "on Domain-specific languages"))
  (define/short dyla "DYLA" (string-append Workshop "on Dynamic Languages and Applications"))
  (define/short ecoop "ECOOP" (string-append "European " Conference "on Object-Oriented Programming"))
  (define/short esop "ESOP" (string-append "European " Symposium "on Programming"))
  (define/short fscd "FSCD" (string-append International Conference "on Formal Structures for Computation and Deduction"))
  (define/short flops "FLOPS" (string-append Symposium "Functional and Logic Programming"))
  (define/short foal "FOAL" "Foundations of Aspect-Oriented Languages")
  (define/short fool "FOOL" (string-append International Workshop "on Foundations of Object-Oriented Languages"))
  (define/short fpca "FPCA" (string-append ACM International Conference "on Functional Programming Languages and Computer Architecture"))
  (define/short fse "FSE" (string-append International Symposium "on the Foundations of Software Engineering"))
  (define/short gpce "GPCE" "Generative Programming: Concepts & Experiences")
  (define/short haskell "Haskell Workshop")
  (define/short haskells "Haskell" (string-append ACM Symposium "on Haskell"))
  (define/short hosc "HOSC" "Higher-Order and Symbolic Computation")
  (define/short i&c "Info. & Comp." "Information and Computation")
  (define/short icalp "ICALP" (string-append International "Colloquium on Automata, Languages, and Programming"))
  (define/short icfp "ICFP" (string-append ACM International Conference "on Functional Programming"))
  (define/short iclp "ICLP" (string-append  International Conference "on Logic Programming"))
  (define/short icse "ICSE" (string-append International Conference "on Software Engineering"))
  (define/short ieee-software (string-append IEEE "Software"))
  (define/short ifl "IFL" (string-append International Symposium "Functional and Logic Programming"))
  (define/short ip "Information Processing" "Information Processing")
  (define/short issta "ISSTA" (string-append International Symposium "on Software Testing and Analysis"))
  (define/short jfp "JFP" (string-append Journal "Functional Programming"))
  (define/short jcss "JCSS" (string-append Journal "of Computer and System Sciences"))
  (define/short jsl "JSL" (string-append Journal "of Symbolic Logic"))
  (define/short lfp "LFP" "LISP and Functional Programming")
  (define/short pacmpl "PACMPL" "Proceedings of the ACM on Programming Languages")
  (define/short lncs "LNCS" "Lecture Notes in Computer Science")
  (define/short lsc "LSC" "LISP and Symbolic Computation")
  (define/short lp "LP" (string-append Workshop "on Logic of Programs"))
  (define/short ml-workshop "ML" (string-append Workshop "on ML"))
  (define/short mscs "MSCS" "Mathematical Structures in Computer Science")
  (define/short oopsla "OOPSLA" (string-append ACM Conference "on Object-Oriented Programming, Systems, Languages and Applications"))
  (define/short onward "Onward!" "Onward!")
  (define/short padl "PADL" (string-append Symposium "on Practical Aspects of Declarative Languages"))
  (define/short pepm "PEPM" (string-append ACM Workshop "on Partial Evaluation and Program Manipulation"))
  (define/short pldi "PLDI" (string-append ACM Conference "on Programming Language Design and Implementation"))
  (define/short plpv "PLPV" (string-append ACM Workshop "Programming Languages meets Program Verification"))
  (define/short popl "POPL" (string-append ACM Symposium "on Principles of Programming Languages"))
  (define/short sac "SAC" (string-append Symposium "on Applied Computing"))
  (define/short sas "SAS" (string-append International Symposium "on Static Analysis"))
  (define/short sblp "SBLP" "Brazilian Symposium on Programming Languages")
  (define/short scp "SCP" "Science of Computer Programming")
  (define/short scala "SCALA" (string-append Workshop "on Scala"))
  (define/short scheme-workshop "SFP" (string-append "Scheme and Functional Programming Workshop"))
  (define/short snapl "SNAPL" "Summit oN Advances in Programming Languages")
  (define/short sigmod "SIGMOD" (string-append ACM "SIGMOD " International Conference "on Management of Data"))
  (define/short sigplan-notices "SIGPLAN Notices" (string-append ACM "SIGPLAN Notices"))
  (define/short tacas (string-append International Conference "on Tools and Algorithms for the Construction and Analysis of Systems"))
  (define/short tacs (string-append International Symposium "Theoretical Aspects of Computer Science"))
  (define/short tcs "Theoretical Computer Science")
  (define/short tfp "TFP" (string-append Symposium "Trends in Functional Programming"))
  (define/short tlca "TLCA" (string-append International Conference "Typed Lambda Calculi and Applications"))
  (define/short toplas "TOPLAS" (string-append Transactions "on Programming Languages and Systems"))
  (define/short types (string-append International Workshop "on Types for Proofs and Programs"))
) (require 'util)

;; ----------------------------------------
;; The original papers

(define st-sfp-2006
  (make-bib
   #:title "Gradual Typing for Functional Languages"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location @elem{@italic{Scheme and Functional Programming}. University of Chicago, TR-2006-06}
   #:date 2006))

(define tf-dls-2006
  (make-bib
   #:title "Interlanguage Migration: from Scripts to Programs"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location dls #:pages '(964 974))
   #:date 2006))

(define gktff-sfp-2006
  (make-bib
   #:title "Sage: Hybrid Checking for Flexible Specifications"
   #:author (authors "Jessica Gronski" "Kenneth Knowles" "Aaron Tomb"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:location (techrpt-location #:institution "University of Chicago" #:number "TR-2006-06")
   #:date 2006))

(define kf-toplas-2010
  (make-bib
    #:title "Hybrid Type Checking"
    #:author (authors "Kenneth Knowles" "Cormac Flanagan")
    #:location (journal-location toplas #:volume 32 #:number 6 #:pages '(1 34))
    #:date 2010))

;; ----------------------------------------
;; Subsequent work

(define ktgff-tech-2007
  (make-bib
   #:title "Sage: Unified Hybrid Checking for First-Class Types, General Refinement Types, and Dynamic (Extended Report)"
   #:author (authors "Kenneth Knowles" "Aaron Tomb" "Jessica Gronski"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:date 2007))

(define st-ecoop-2007
  (make-bib
   #:title "Gradual Typing for Objects"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location (proceedings-location ecoop #:pages '(2 27))
   #:date 2007))

(define ctf-sfp-2007
  (make-bib
   #:title "Advanced Macrology and the Implementation of Typed Scheme"
   #:author (authors "Ryan Culpepper" "Sam Tobin-Hochstadt" "Matthew Flatt")
   #:location (proceedings-location scheme-workshop #:pages '(1 13))
   #:date 2007))

(define hansen-tech-2007
  (make-bib
   #:title "Evolutionary Programming and Gradual Typing in ECMAScript 4"
   #:author "Lars T. Hansen"
   #:date 2007))

(define hf-ml-2007
  (make-bib
   #:title "Status report: specifying JavaScript with ML"
   #:author (authors "David Herman" "Cormac Flanagan")
   #:location (proceedings-location ml-workshop #:pages #false)
   #:date 2007))

(define tf-popl-2008
  (make-bib
   #:title "The Design and Implementation of Typed Scheme"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date 2008))

(define gray-ecoop-2008
  (make-bib
   #:title "Safe Cross-Language Inheritance"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location ecoop #:pages '(52 75))
   #:date 2008))

(define sv-dls-2008
  (make-bib
   #:title "Gradual Typing with Unification-based Inference"
   #:author (authors "Jeremy G. Siek" "Manish Vachharajani")
   #:location (proceedings-location dls #:pages #false)
   #:date 2008))

(define wf-esop-2009
  ;; previously appeared in STOP 2007
  (make-bib
   #:title "Well-typed Programs Can't be Blamed"
   #:author (authors "Philip Wadler" "Robert Bruce Findler")
   #:location (proceedings-location esop #:pages '(1 15))
   #:date 2009))

(define sgt-esop-2009
  (make-bib
   #:title "Exploring the Design Space of Higher-Order Casts"
   #:author (authors "Jeremy G. Siek" "Ronald Garcia" "Walid Taha")
   #:location (proceedings-location esop #:pages '(17 31))
   #:date 2009))

(define gray-chapter-2009
  (make-bib
   #:title "A Model of Java/Scheme Interoperability"
   #:author "Kathryn E. Gray"
   #:date 2009))

(define mf-toplas-2009
  (make-bib
   #:title "Operational Semantics for Multi-language Programs"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '(1 44))
   #:date 2009))

(define ii-cs-2009
  (make-bib
   #:title "Gradual Typing for Featherweight Java"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (journal-location "Computer Software"
                                #:volume 26
                                #:number 2
                                #:pages '(18 40))
   #:date 2009))

(define shb-icfp-2009
  (make-bib
   #:title "A Theory of Typed Coercions and its Applications"
   #:author (authors "Nikhil Swamy" "Michael Hicks" "Gavin M. Bierman")
   #:location (proceedings-location icfp #:pages '(329 340))
   #:date 2009))

(define bfnorsvw-oopsla-2009
  (make-bib
   #:title "Thorn: Robust, Concurrent, Extensible Scripting on the JVM"
   #:author (authors "Bard Bloom" "John Field" "Nathaniel Nystrom"
                     "Johan Östlund" "Gregor Richards" "Rok Strniša"
                     "Jan Vitek" "Tobias Wrigstad")
   #:location (proceedings-location oopsla #:pages '(117 136))
   #:date 2009))

(define furr-dissertation-2009
  (make-bib
   #:title "Combining Static and Dynamic Typing in Ruby"
   #:author "Michael Furr"
   #:location (dissertation-location #:institution "University of Maryland"
                                     #:degree "Ph.D.")
   #:date 2009))

(define tobin-hochstadt-dissertation-2010
  (make-bib
   #:title "Typed Scheme: From Scripts to Programs"
   #:author "Sam Tobin-Hochstadt"
   #:location (dissertation-location #:institution "Northeastern University"
                                     #:degree "Ph.D.")
   #:date 2010))

(define sk-2001
  (make-bib
   #:title "Linguistic Reuse"
   #:author "Shriram Krishnamurthi"
   #:location (dissertation-location #:institution "Rice University" #:degree "Ph.D.")
   #:date 2010))

(define wnlov-popl-2010
  (make-bib
   #:title "Integrating Typed and Untyped Code in a Scripting Language"
   #:author (authors "Tobias Wrigstad" "Francesco Zappa Nardelli" "Sylvain Lebresne" "Johan Östlund" "Jan Vitek")
   #:location (proceedings-location popl #:pages '(377 388))
   #:date 2010))

(define sw-popl-2010
  (make-bib
   #:title "Threesomes, with and without blame"
   #:author (authors "Jeremy G. Siek" "Philip Wadler")
   #:location (proceedings-location popl #:pages '(365 376))
   #:date 2010))

(define htf-hosc-2010
  ;; previously appeared in TFP 2007
  (make-bib
   #:title "Space-efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (journal-location hosc
                                #:volume 23
                                #:number 2
                                #:pages '(167 189))
   #:date 2010))

(define bmt-ecoop-2010
  (make-bib
   #:title "Adding Dynamic Types to C#"
   #:author (authors "Gavin Bierman" "Erik Meijer" "Mads Torgersen")
   #:location (proceedings-location ecoop #:pages '(76 100))
   #:date 2010))

(define gray-fool-2010
  (make-bib
   #:title "Interoperability in a Scripted World: Putting Inheritance and Prototypes Together"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location fool #:pages #false)
   #:date 2010))

(define afsw-popl-2011
  (make-bib
   #:author (authors "Amal Ahmed" "Robert Bruce Findler"
                     "Jeremy G. Siek" "Philip Wadler")
   #:title "Blame for All"
   #:location (proceedings-location popl #:pages '(201 214))
   #:date 2011))

(define thscff-pldi-2011
  (make-bib
   #:title "Languages as Libraries"
   #:author (authors "Sam Tobin-Hochstadt" "Vincent St-Amour"
                     "Ryan Culpepper" "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(132 141))
   #:date 2011))

(define bce-icse-2011
  (make-bib
   #:title "Always-available Static and Dynamic Feedback"
   #:author (authors "Michael Bayne" "Richard Cook" "Michael D. Ernst")
   #:location (proceedings-location icse #:pages '(521 530))
   #:date 2011))

(define wgta-ecoop-2011
  (make-bib
   #:title "Gradual Typestate"
   #:author (authors "Roger Wolff" "Ronald Garcia"
                     "Éric Tanter" "Jonathan Aldritch")
   #:location (proceedings-location ecoop #:pages '(459 483))
   #:date 2011))

(define ii-oopsla-2011
  (make-bib
   #:title "Gradual Typing for Generics"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (proceedings-location oopsla #:pages '(609 624))
   #:date 2011))

(define cmscgbwf-dls-2011
  (make-bib
   #:title "The Impact of Optional Type Information on JIT Compilation of Dynamically Typed Languages"
   #:author (authors "Mason Chang" "Bernd Mathiske"
                     "Edwin Smith" "Avik Chaudhuri"
                     "Andreas Gal" "Michael Bebenita"
                     "Christian Wimmer" "Michael Franz")
   #:location (proceedings-location dls #:pages '(13 24))
   #:date 2011))

(define rch-popl-2012
  (make-bib
   #:title "The Ins and Outs of Gradual Type Inference"
   #:author (authors "Aseem Rastogi" "Avik Chaudhuri" "Basil Hosmer")
   #:location (proceedings-location popl #:pages '(481 494))
   #:date 2012))

(define dtf-esop-2012
  (make-bib
   #:title "Complete Monitors for Behavioral Contracts"
   #:author (authors "Christos Dimoulas" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(214 233))
   #:date 2012))

(define stff-oopsla-2012
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

(define tsdtf-oopsla-2012
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland"
                     "Christos Dimoulas" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Gradual Typing for First-Class Classes"
   #:location (proceedings-location oopsla #:pages '(793 810))
   #:date 2012))

(define tsth-esop-2013
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (proceedings-location esop #:pages '(229 248))
   #:date "2013"))

(define acftd-scp-2013
  (make-bib
   #:author (authors "Esteban Allende" "Oscar Callaú" "Johan Fabry" "Éric Tanter" "Marcus Denker")
   #:title "Gradual typing for Smalltalk"
   #:location (journal-location scp
                                #:volume 96
                                #:number 1
                                #:pages '(52 69))
   #:date 2013))

(define aft-dls-2013
  (make-bib
   #:author (authors "Esteban Allende" "Johan Fabry" "Éric Tanter")
   #:title "Cast Insertion Strategies for Gradually-Typed Objects"
   #:location (proceedings-location dls #:pages '(27 36))
   #:date 2013))

(define vksb-dls-2014
  (make-bib
   #:author (authors "Michael M. Vitousek" "Andrew Kent" "Jeremy G. Siek" "Jim Baker")
   #:title "Design and Evaluation of Gradual Typing for Python"
   #:location (proceedings-location dls #:pages '(45 56))
   #:date 2014))

(define afgt-oopsla-2014
  (make-bib
   #:author (authors "Esteban Allende" "Johan Fabry" "Ronald Garcia" "Éric Tanter")
   #:title "Confined Gradual Typing"
   #:location (proceedings-location oopsla #:pages '(251 270))
   #:date 2014))

(define rsfbv-popl-2015
  (make-bib
   #:author (authors "Aseem Rastogi" "Nikhil Swamy" "Cédric Fournet"
                     "Gavin Bierman" "Panagiotis Vekris")
   #:title "Safe & Efficient Gradual Typing for TypeScript"
   #:location (proceedings-location popl #:pages '(167 180))
   #:date 2015))

(define gc-popl-2015
  (make-bib
   #:author (authors "Ronald Garcia" "Matteo Cimini")
   #:title "Principal Type Schemes for Gradual Programs"
   #:location (proceedings-location popl #:pages '(303 315))
   #:date 2015))

(define svctg-esop-2015
  (make-bib
   #:title "Monotonic References for Efficient Gradual Typing"
   #:location (proceedings-location esop #:pages '(432 456))
   #:date 2015
   #:author (authors "Jeremy Siek"
                     "Michael M. Vitousek"
                     "Matteo Cimmini"
                     "Sam Tobin-Hochstadt"
                     "Ronald Garcia")))

(define tfdfftf-ecoop-2015
  (make-bib
   #:author (authors "Asumu Takikawa" "Daniel Feltey"
                     "Earl Dean" "Robert Bruce Findler"
                     "Matthew Flatt" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Towards Practical Gradual Typing"
   #:location (proceedings-location ecoop #:pages '(4 27))
   #:date 2015))

(define tfgnvf-popl-2016
  (make-bib
   #:author (authors "Asumu Takikawa"
                     "Daniel Feltey"
                     "Ben Greenman"
                     "Max S. New"
                     "Jan Vitek"
                     "Matthias Felleisen")
   #:title "Is Sound Gradual Typing Dead?"
   #:location (proceedings-location popl #:pages '(456 468))
   #:date 2016))

(define ccew-popl-2018
  (make-bib
    #:author (authors "John Peter Campora" "Sheng Chen" "Martin Erwig" "Eric Walkingshaw")
    #:title "Migrating Gradual Types"
    #:location (journal-location pacmpl #:volume 2 #:number "POPL" #:pages '(15:1 15:29))
    #:date 2018))

(define gf-icfp-2018
  (make-bib
    #:author (authors "Ben Greenman" "Matthias Felleisen")
    #:title "A Spectrum of Type Soundness and Performance"
    #:location (journal-location pacmpl #:volume 1 #:number "ICFP" #:pages '(71:1 71:27))
    #:date 2018))

(define gtnffvf-jfp-2017
  (make-bib
   #:author (authors "Ben Greenman"
                     "Asumu Takikawa"
                     "Max S. New"
                     "Daniel Feltey"
                     "Robert Bruce Findler"
                     "Jan Vitek"
                     "Matthias Felleisen")
   #:title "How to Evaluate the Performance of Gradual Type Systems"
   #:location "Submitted for publication"
   #:date 2016))

(define rzv-ecoop-2015
  (make-bib
   #:author (authors "Gregor Richards" "Zappa Nardelli, Francesco" "Jan Vitek")
   #:title "Concrete Types for TypeScript"
   #:location (proceedings-location ecoop #:pages '(76 100))
   #:date 2015))

;; ----------------------------------------
; Early Work on Interoperation

(define ff-icfp-2002
  (make-bib
   #:title "Contracts for Higher-Order Functions"
   #:author (authors "Robert Bruce Findler" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(48 59))
   #:date "2002"))

;; ----------------------------------------
; Contracts

(define gf-tfp-2007
  (make-bib
   #:title "Unifying Hybrid Types and Contracts"
   #:author (authors "Jessica Gronski" "Cormac Flanagan")
   #:location (proceedings-location tfp #:pages #false)
   #:date 2007))

(define bgip-esop-2011
  (make-bib
   #:title "Polymorphic Contracts"
   #:author (authors "João Filipe Belo" "Michael Greenberg"
                     "Atsushi Igarashi" "Benjamin C. Pierce")
   #:location (proceedings-location esop #:pages '(18 37))
   #:date 2011))

;; ----------------------------------------
; Typing Untyped Languages

(define c-icalp-1976
  (make-bib
   #:title "User-defined Data Types as an Aid to Verifying LISP Programs"
   #:author "Robert Cartwright"
   #:location (proceedings-location icalp #:pages '(228 256))
   #:date 1976))

(define s-popl-1981
  (make-bib
   #:title "Inferring Types in Smalltalk"
   #:author "Norihisa Suzuki"
   #:location (proceedings-location popl #:pages '(187 199))
   #:date 1981))

(define st-popl-1984
  (make-bib
   #:title "Creating Efficient Systems for Object-Oriented Languages"
   #:author (authors "Norihisa Suzuki" "Minoru Terada")
   #:location (proceedings-location popl #:pages '(290 296))
   #:date 1984))

(define am-popl-1991
  (make-bib
   #:title "Static Type Inference in a Dynamically Typed Language"
   #:author (authors "Alexander Aiken" "Brian R. Murphy")
   #:location (proceedings-location popl #:pages '(279 290))
   #:date 1991))

(define cf-pldi-1991
  (make-bib
   #:title "Soft Typing"
   #:author (authors "Robert Cartwright" "Mike Fagan")
   #:location (proceedings-location pldi #:pages '(278 292))
   #:date 1991))

(define bg-oopsla-1993
  (make-bib
   #:title "Strongtalk: Typechecking Smalltalk in a Production Environment"
   #:author (authors "Gilad Bracha" "David Griswold")
   #:location (proceedings-location oopsla #:pages '(215 230))
   #:date 1993))

(define awl-popl-1994
  (make-bib
   #:title "Soft Typing with Conditional Types"
   #:author (authors "Alexander Aiken" "Edward L. Wimmers" "T.K. Lakshman")
   #:location (proceedings-location popl #:pages '(163 173))
   #:date 1994))

(define h-scp-1994
  (make-bib
   #:author "Fritz Henglein"
   #:title "Dynamic Typing: Syntax and Proof Theory"
   #:location (journal-location scp
                                #:volume 22
                                #:number 3
                                #:pages '(197 230))
   #:date 1994))

(define hr-fpca-1995
  (make-bib
   #:author (authors "Fritz Henglein" "Jakob Rehof")
   #:title "Safe Polymorphic Type Inference for a Dynamically Typed Language: Translating Scheme to ML"
   #:location (proceedings-location fpca #:pages '(192 203))
   #:date 1995))

(define haynes-tech-1995
  (make-bib
   #:author "Christopher T. Haynes"
   #:title "Infer: a Statically-typed Dialect of Scheme"
   #:location (techrpt-location #:institution "Indiana University"
                                #:number "367")
   #:date 1995))

(define akers-dissertation-1996
  (make-bib
   #:title "Strong Static Type Checking for Functional Common Lisp"
   #:author "Robert Akers"
   #:location (dissertation-location #:institution "University of Texas")
   #:date 1996))

(define fagan-dissertation-1992
  (make-bib
   #:title "Soft Typing: An Approach to Type Checking for Dynamically Typed Languages"
   #:author "Mike Fagan"
   #:location (dissertation-location #:institution "Rice University")
   #:date 1992))

(define ffkwf-pldi-1996
  (make-bib
   #:title "Catching Bugs in the Web of Program Invariants"
   #:author (authors "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Stephanie Weirich"
                     "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(23 32))
   #:date 1996))

(define mw-icfp-1997
  (make-bib
   #:title "A Practical Subtyping System for Erlang"
   #:author (authors "Simon Marlow" "Philip Wadler")
   #:location (proceedings-location icfp #:pages '(136 149))
   #:date 1997))

(define wc-toplas-1997
  (make-bib
   #:title "A Practical Soft Type System for Scheme"
   #:author (authors "Andrew K. Wright" "Robert Cartwright")
   #:location (journal-location toplas
                                #:volume 19
                                #:number 1
                                #:pages '(87 152))
   #:date 1997))

;; ----------------------------------------
; Type Systems for Gradual Typing

(define lg-popl-1988
  (make-bib
   #:title "Polymorphic Effect Systems"
   #:author (authors "John M. Lucassen" "David K. Gifford")
   #:location (proceedings-location popl #:pages '(47 57))
   #:date 1988))

(define tf-icfp-2010
  (make-bib
   #:title "Logical Types for Untyped Languages"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(117 128))
   #:date 2010))

(define stff-padl-2012
  (make-bib
   #:title "Typing the Numeric Tower"
   #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt"
                     "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location padl #:pages '(289 303))
   #:date 2012))

(define fafh-sac-2009
  (make-bib
   #:title "Static Type Inference for Ruby"
   #:author (authors "Michael Furr" "Jong-hoon (David) An"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location sac #:pages '(1859 1866))
   #:date 2009))

(define acfh-popl-2011
  (make-bib
   #:title "Dynamic Inference of Static Types for Ruby"
   #:author (authors "Jong-hoon (David) An" "Avik Chaudhuri"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location popl #:pages '(459 472))
   #:date 2011))

(define chj-oopsla-2012
  (make-bib
   #:title "Dependent Types for JavaScript"
   #:author (authors "Ravi Chugh" "David Herman"
                     "Ranjit Jhala")
   #:location (proceedings-location oopsla #:pages '(587 606))
   #:date 2012))

(define bonnaire-sergeant-thesis-2012
  (make-bib
   #:author "Ambrose Bonnaire-Sergeant"
   #:title "A Practical Optional Type System for Clojure"
   #:location (dissertation-location
               #:institution "University of Western Australia"
               #:degree "Honour's")
   #:date 2012))

(define bdt-esop-2016
  (make-bib
   #:title "Practical Optional Types for Clojure"
   #:author (authors "Ambrose Bonnaire-Sergeant" "Rowan Davies" "Sam Tobin-Hochstadt")
   #:location (proceedings-location esop #:pages '(68 94))
   #:date 2016))

(define mmi-dyla-2014
  (make-bib
   #:title "Typed Lua: An Optional Type System for Lua"
   #:author (authors "André Murbach Maidl" "Fabio Mascarenhas" "Roberto Ierusalimschy")
   #:location (proceedings-location dyla #:pages '(1 10))
   #:date 2014))

(define mmi-dls-2015
  (make-bib
    #:title "A Formalization of Typed Lua"
    #:author (authors "Andre Murbach Maidl" "Fabio Mascarenhas" "Roberto Ierusalimschy")
    #:location (proceedings-location dls #:pages '(13 25))
    #:date 2015))

(define clojure-macros
  (make-bib
   #:date 2016
   #:author "Clojure 1.8.0"
   #:title "Macros"
   #:url "http://clojure.org/reference/macros"))

(define rust-compiler-plugins
  (make-bib
   #:date 2016
   #:author "Rust 1.7"
   #:title "Compiler Plugins"
   #:url "https://doc.rust-lang.org/book/compiler-plugins.html"))

(define gal-firefox
  (make-bib
   #:date 2010
   #:author "Andreas Gal"
   #:title "Proxies in Tracemonkey"
   #:url "http://hg.mozilla.org/tracemonkey/"))

(define Eiffel
  (make-bib
   #:author "Bertrand Meyer"
   #:title "Eiffel : The Language"
   #:is-book? #t
   #:date "1991"
   #:location (book-location #:publisher "Prentice Hall PTR")))

(define AmbientTalk
  (make-bib
   #:title "Ambient-Oriented Programming"
   #:author (authors "Jessie Dedecker"
                     (author-name "Tom" "Van Cutsem")
                     "Stijn Mostinckx"
                     "Theo D'Hondt"
                     (author-name "Wolfgang" "De Meuter"))
   #:date "2005"
   #:location (proceedings-location oopsla
                                    #:pages '(31 40))))

(define Euclid
  (make-bib
   #:author (authors "B. W. Lampson"
                     "J. J. Horning"
                     "R. L. London"
                     "J. G. Mitchell"
                     "G. J. Popek")
   #:title "Report on the programming language Euclid"
   #:location (journal-location sigplan-notices
                                #:volume 12
                                #:number 2
                                #:pages '(1 79))
   #:date "1977"))


(define Anna
  (make-bib
   #:author (authors "D. C. Luckham"
                     "F. W. von Henke")
   #:title "An overview of Anna, a specification language for Ada"
   #:location (journal-location ieee-software
                                #:volume 2
                                #:number 2
                                #:pages '(9 22))
   #:date "1985"))


(define D
  (make-bib
   #:author (org-author-name "Digital Mars")
   #:date "1999"
   #:title "D Programming Language"
   #:url "http://www.digitalmars.com/d/"))

(define lm-fpca-1991
  (make-bib
    #:title "Dynamics in ML"
    #:author (authors "Xavier Leroy" "Michael Mauny")
    #:location (proceedings-location fpca #:pages '(406 426))
    #:date 1991))

(define t-popl-1990
  (make-bib
    #:title "Quasi-static Typing"
    #:author "Satish Thatte"
    #:location (proceedings-location popl #:pages '(367 381))
    #:date 1990))

(define acpp-toplas-1991
  (make-bib
    #:title "Dynamic Typing in a Statically Typed Language"
    #:author (authors "Martin Abadi"
                      "Luca Cardelli"
                      "Benjamin C. Pierce"
                      "Gordon D. Plotkin")
    #:date 1991
    #:location (journal-location toplas
    #:volume 13
    #:number 2
    #:pages '("237" "268"))))

(define sf-ifl-2009
  (make-bib
   #:title "Nested and Dynamic Contract Boundaries"
   #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
   #:location (proceedings-location ifl
                                    #:pages '(141 158))
   #:date "2009"))

 (define sf-dls-2010
   (make-bib
    #:title "Contracts for First-Class Classes"
    #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
    #:location (proceedings-location dls
                                     #:pages '(97 112))
    #:date "2010"))

(define vvle-tr
  (make-bib
   #:title "Virtual Values for Language Extension"
   #:author (authors "Thomas H. Austin" "Tim Disney" "Cormac Flanagan")
   #:location (proceedings-location oopsla #:pages #false)
   #:date "2011"))

(define ContractsCoffee
  (make-bib
   #:title "Contracts.coffee"
   #:author "Tim Disney"
   #:date "2012"
   #:url "http://disnetdev.com/contracts.coffee/"))

(define redex-book
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

(define sfp2009-kf
  (make-bib
   #:author (authors "Casey Klein" "Robert Bruce Findler")
   #:title "Randomized Testing in PLT Redex"
   #:location (proceedings-location scheme-workshop #:pages '(26 36))
   #:date "2009"))

(define poly-sealing
  (make-bib
   #:title "Parametric Polymorphism Through Run-Time Sealing, or, Theorems for Low, Low Prices!"
   #:author (authors "Jacob Matthews" "Amal Ahmed")
   #:location (proceedings-location esop #:series "LNCS 4960" #:pages '(16 31))
   #:date 2008))

(define ciao-contracts
  (make-bib
   #:author (authors "E. Mera" "P. Lopez-Garcia" "M. Hermenegildo")
   #:title "Integrating Software Testing and Run-Time Checking in an Assertion Verification Framework"
   #:location (proceedings-location iclp #:series "LNCS 5649" #:pages #false)
   #:date 2009))

(define dfff-popl-2011
  (make-bib
   #:title "Correct Blame for Contracts: No More Scapegoating"
   #:author (authors "Christos Dimoulas" "Robert Bruce Findler" "Cormac Flanagan" "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(215 226))
   #:date 2011))

(define dw-popl-2006
  (make-bib
   #:title "Harmless Advice"
   #:author (authors "Daniel S. Dantas" "David Walker")
   #:location (proceedings-location popl #:pages '(383 396))
   #:date 2006))

(define rsb-fse-2004
  (make-bib
   #:title "A Classification System and Analysis for Aspect-Oriented Programs"
   #:author (authors "Martin Rinard" "Alexandru Salcianu" "Suhabe Bugrara")
   #:location (proceedings-location fse #:pages #false)
   #:date 2004))

(define cl-foal-2002
  (make-bib
   #:author (authors "Curtis Clifton" "Gary T. Leavens")
   #:title "Observers and assistants: A proposal for modular aspect-oriented reasoning"
   #:location (proceedings-location foal #:pages #false)
   #:date 2002))

(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-1")
   #:url      "http://racket-lang.org/tr1/"))

(define plt-tr3
  (make-bib
   #:title "GUI: Racket Graphics Toolkit"
   #:author (authors "Matthew Flatt" "Robert Bruce Findler" "John Clements")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-3")
   #:url      "http://racket-lang.org/tr3/"))

(define EffectiveAdvice
  (make-bib
   #:title "EffectiveAdvice: Disciplined Advice with Explicit Effects"
   #:author (authors (author-name "Bruno C. d. S." "Oliveira")
                     "Tom Schrijvers"
                     "William R. Cook")
   #:date 2010
   #:location (proceedings-location aosd #:pages #false)))

(define OpenModules
  (make-bib
   #:title "Open Modules: Modular Reasoning About Advice"
   #:author "Jonathan Aldrich"
   #:date 2005
   #:location (proceedings-location ecoop #:pages #false)))

(define MillerPhD
  (make-bib
   #:title "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control"
   #:author "M. S. Miller"
   #:is-book? #t
   #:location (dissertation-location #:institution "John Hopkins University")
   #:date 2006))

(define hg-pldi-2012
  (make-bib #:title "Fast and precise type inference for JavaScript"
            #:author (authors (author-name "Brian" "Hackett")
                              (author-name "Shu-Yu" "Guo"))
            #:location (proceedings-location pldi #:pages '(239 250))
            #:date "2012"))

(define min-cover-salient-curves
  (make-bib
   #:author (authors "Pedro Felzenszwalb" "David McAllester")
   #:title "A min-cover approach for finding salient curves"
   #:location (proceedings-location (string-append IEEE Workshop "Perceptual Organization in Computer Vision") #:pages #false)
   #:date 2006))

(define wf-ic-1994
  (make-bib
   #:author (authors "Andrew K. Wright" "Matthias Felleisen")
   #:title "A Syntactic Approach to Type Soundness"
   #:location (journal-location i&c #:pages '(38 94))
   #:date 1994))

(define f-scp-1991
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "On the Expressive Power of Programming Languages"
   #:location (journal-location scp
                                #:volume 17
                                #:number "1--3"
                                #:pages '(35 75))
   #:date 1991))

;; ----------------------------------------
; Software engineering

(define crtr-empirical-2013
  (make-bib
   #:author (authors "Oscar Callaú" "Romain Robbes"
                     "Éric Tanter" "David Röthlisberger")
   #:title "How (and Why) Developers Use the Dynamic Features of Programming Languages: the Case of Smalltalk"
   #:location (journal-location "Empirical Software Engineering"
                                #:volume 18
                                #:number 6
                                #:pages '(1156 1194))
   #:date 2013))

(define cm-tech-1985
  (make-bib
   #:author (authors "Robert L. Constable" "Nax P. Mendler")
   #:title "Recursive Definitions in Type Theory"
   #:location (techrpt-location #:institution "Cornell University"
                                #:number "TR 85-659")
   #:date 1985))

(define mff-popl-2006
  (make-bib
   #:author (authors "Philippe Meunier" "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Modular Set-Based Analysis from Contracts"
   #:location (proceedings-location popl #:pages '(218 231))
   #:date 2006))

(define hkrts-empirical-2013
  (make-bib
   #:author (authors "Stefan Hanenberg" "Sebastian Kleinschmager"
                     "Romain Robbes" "Éric Tanter" "Andreas Stefik")
   #:title "An Empirical Study on the Impact of Static Typing on Software Maintainability"
   #:location (journal-location "Empirical Software Engineering"
                                ;; there really isn't any volume/number listed on Springer
                                #:pages '(1 48))
   #:date 2013))

;; ----------------------------------------
; Objects, theory

(define ac-book-1996
  (make-bib
   #:author (authors "Martin Abadi" "Luca Cardelli")
   #:title "A Theory of Objects"
   #:date 1996
   #:is-book? #t
   #:location (book-location #:publisher "Springer-Verlag")))

;; ----------------------------------------
; Objects, real languages

(define remy-tacs-1994
  (make-bib
    #:author "Didier Rémy"
    #:title "Programming Objects with ML-ART an Extension to ML with Abstract and Record Types"
    #:date 1994
    #:location (proceedings-location tacs #:pages '(321 346))))

(define oacddemmmsssz-tech-2006
  (make-bib
   #:title "An Overview of the Scala Programming Language"
   #:author (authors "Martin Odersky" "Philippe Altherr"
                     "Vincent Cremet" "Iulian Dragos"
                     "Gilles Dubochet" "Burak Emir"
                     "Sean McDirmid" "Stéphane Micheloud"
                     "Nikolay Mihaylov" "Michel Schinz"
                     "Erik Stenman" "Lex Spoon"
                     "Matthias Zenger")
   #:date 2006
   #:location (techrpt-location #:institution "École Polytechnique Fédérale de Lausanne"
                                #:number "LAMP-REPORT-2006-001")))

(define mme-gpce-2013
  (make-bib
   #:title "Template Constructors for Resuable Object Initialization"
   #:author (authors "Marko Martin" "Mira Mezini" "Sebastian Erdweg")
   #:location (proceedings-location gpce #:pages '(43 52))
   #:date 2013))

;; ----------------------------------------
; Macrology

(define ts-tcs-2000
  (make-bib
   #:title "MetaML and Multi-stage Programming with Explicit Annotations"
   #:author (authors "Walid Taha" "Tim Sheard")
   #:location (journal-location tcs
                                #:volume 248
                                #:number "1-2"
                                #:pages '(211 242))
   #:date 2000))

(define spj-haskell-2002
  (make-bib
   #:title "Template Meta-programming for Haskell"
   #:author (authors "Tim Sheard" "Simon Peyton Jones")
   #:location (proceedings-location haskell #:pages #false)
   #:date 2002))

;; ----------------------------------------
; Contracts

(define df-toplas-2011
  (make-bib
   #:author (authors "Christos Dimoulas" "Matthias Felleisen")
   #:title "On Contract Satisfaction in a Higher-Order World"
   #:location (journal-location toplas
                                #:volume 33
                                #:number 5
                                #:pages '("16:1" "16:29"))
   #:date 2011))

(define dimoulas-diss
  (make-bib
   #:author "Christos Dimoulas"
   #:title "Foundations for Behavioral Higher-Order Contracts"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2012))

(define nthvh-icfp-2014
  (make-bib
   #:author (authors "Phúc C. Nguyễn" "Sam Tobin-Hochstadt" "David Van Horn")
   #:title "Soft Contract Verification"
   #:location (proceedings-location icfp #:pages '(139 152))
   #:date 2014))

;; ----------------------------------------
; Proxies

(define chaperones-impersonators
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

;; ----------------------------------------
; Continuations

(define ConstrainingControl
  (make-bib
   #:author (authors "Daniel P. Friedman" "Christopher T. Haynes")
   #:title "Constraining Control"
   #:location (proceedings-location popl #:pages '(245 254))
   #:date 1985))

(define ContinuationMultiprocessing
  (make-bib
   #:author (authors "Mitchell Wand")
   #:title "Continuation-Based Multiprocessing"
   #:location (journal-location hosc
                                #:volume 12
                                #:number 3
                                #:pages '(285 299))
   #:date 1999))

(define Engines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman")
   #:title "Engines Build Process Abstractions"
   #:location (proceedings-location lfp #:pages '(18 24))
   #:date 1984))

(define Coroutines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman" "Mitchell Wand")
   #:title "Continuations and Coroutines"
   #:location (proceedings-location lfp #:pages '(293 298))
   #:date 1984))

(define GuilePrompts
  (make-bib
   #:author "Free Software Foundation"
   #:date 2012
   #:title "Guile Reference Manual: Prompts"
   #:url "http://www.gnu.org/software/guile/manual/html_node/Prompts.html"))

;; ----------------------------------------
; Continuation marks / dynamic binding

(define algebraic-stepper
  (make-bib
   #:author (authors "John Clements" "Matthew Flatt" "Matthias Felleisen")
   #:title "Modeling an Algebraic Stepper"
   #:location (proceedings-location esop #:pages '(320 334))
   #:date 2001))

(define DDBinding
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan" "Amr Sabry")
   #:title "Delimited Dynamic Binding"
   #:location (proceedings-location icfp #:pages '(26 37))
   #:date 2006))

(define GenStackInspection
  (make-bib
   #:author (authors "Greg Pettyjohn" "John Clements" "Joe Marshall"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Continuations from Generalized Stack Inspection"
   #:location (proceedings-location icfp #:pages '(216 227))
   #:date 2005))

(define AOPinHOLs
  (make-bib
   #:author (authors "David B. Tucker" "Shriram Krishnamurthi")
   #:title "Pointcuts and Advice in Higher-Order Languages"
   #:location (proceedings-location aosd #:pages '(158 167))
   #:date 2003))

(define CMsinJS
  (make-bib
   #:author (authors "John Clements" "Ayswarya Sundaram" "David Herman")
   #:title "Implementing Continuation Marks in Javascript"
   #:location (proceedings-location scheme-workshop #:pages #false)
   #:date 2008))

(define clements-diss
  (make-bib
   #:title "Portable and High-level Access to the Stack with Continuation Marks"
   #:author "John Clements"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2006))

;; ----------------------------------------
; Delim control

(define Felleisen88
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "The Theory and Practice of First-Class Prompts"
   #:location (proceedings-location popl #:pages '(180 190))
   #:date 1988))

(define Sitaram1993
  (make-bib
   #:author (authors "Dorai Sitaram")
   #:title "Handling Control"
   #:location (proceedings-location pldi #:pages '(147 155))
   #:date 1993))

(define DelimCompControl
  (make-bib
   #:author (authors "Matthew Flatt" "Gang Yu"
                     "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Adding Delimited and Composable Control to a Production Programming Environment"
   #:location (proceedings-location icfp #:pages '(165 176))
   #:date 2007))

(define Subcontinuations
  (make-bib
   #:author (authors "Robert Hieb" "R. Kent Dybvig" "Claude W. Anderson")
   #:title "Subcontinuations"
   #:location (journal-location lsc #:pages '(83 110))
   #:date 1994))

(define DelimiterHierarchies
  (make-bib
   #:author (authors "Dorai Sitaram" "Matthias Felleisen")
   #:title "Control Delimiters and their Hierarchies"
   #:location (journal-location lsc #:pages '(67 99))
   #:date 1990))

(define MachIV
  (make-bib
   #:author "Richard P. Draves"
   #:title "Control Transfer in Operating System Kernels"
   #:is-book? #t
   #:location (dissertation-location #:institution "Carnegie Mellon University")
   #:date 1994))

;; ----------------------------------------
; Types for untyped languages

(define rtsf-sac-2013
  (make-bib
   #:author (authors "Brianna M. Ren" "John Toman" "T. Stephen Strickland" "Jeffrey S. Foster")
   #:title "The Ruby Type Checker"
   #:location (proceedings-location sac #:pages '(1565 1572))
   #:date 2013))

;; ----------------------------------------
; Gradual typing

;; for these papers, see
;; https://github.com/samth/gradual-typing-bib/blob/master/main.rkt

(define typescript
  (make-bib
   #:title "Typescript Language Specification"
   #:location (techrpt-location #:number "Version 0.9.1"
                                #:institution "Microsoft")
   #:date 2013))

;; ----------------------------------------
; Components and modules

(define mfh-oopsla-2001
  (make-bib
   #:title "Jiazzi: New-Age Components for Old-Fashioned Java"
   #:author (authors "Sean McDirmid" "Matthew Flatt"
                     "Wilson C. Hsleh")
   #:date 2001
   #:location (proceedings-location oopsla #:pages '(211 222))))

(define fg-ml-2010
  (make-bib
   #:title "First-class Modules and Composable Signatures in Objective Caml 3.12"
   #:author (authors "Alain Frisch" "Jacques Garrique")
   #:date 2010
   #:location (proceedings-location ml-workshop #:pages #false)))

;; ----------------------------------------
; Mixins and traits

(define fkf-popl-1998
  (make-bib
   #:title "Classes and Mixins"
   #:author (authors "Matthew Flatt" "Shriram Krishnamurthi"
                     "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(171 183))
   #:date 1998))

(define alz-ecoop-2000
  (make-bib
   #:title "Jam - A Smooth Extension of Java with Mixins"
   #:author (authors "Davide Ancona" "Giovanni Lagorio" "Elena Zucca")
   #:location (proceedings-location esop #:pages '(154 178))
   #:date 2000))

(define abc-oopsla-2003
  (make-bib
   #:title "A First-class Approach to Genericity"
   #:author (authors "Eric Allen" "Jonathan Bannet"
                     "Robert Cartwright")
   #:location (proceedings-location oopsla #:pages '(96 114))
   #:date 2003))

(define sdnb-ecoop-2003
  (make-bib
   #:title "Traits: Composable Units of Behaviour"
   #:author (authors "Nathanael Schärli" "Stéphane Ducasse"
                     "Oscar Nierstrasz" "Andrew P. Black")
   #:location (proceedings-location ecoop #:pages '(248 274))
   #:date 2003))

(define kt-asplas-2004
  (make-bib
   #:title "McJava – A Design and Implementation of Java with Mixin-Types"
   #:author (authors "Tetsuo Kamina" "Tetsuo Tamai")
   #:location (proceedings-location asplas #:pages '(398 414))
   #:date 2004))

(define sd-ecoop-2005
  (make-bib
   #:title "Chai: Traits for Java-Like Languages"
   #:author (authors "Charles Smith" "Sophia Drossopoulou")
   #:location (proceedings-location ecoop #:pages '(453 478))
   #:date 2005))

(define sz-oopsla-2010
  (make-bib
   #:title "MetaFJig: a Meta-circular Composition Language for Java-like Classes"
   #:author (authors "Marco Servetto" "Elena Zucca")
   #:location (proceedings-location oopsla #:pages '(464 483))
   #:date 2010))

;; ----------------------------------------
; Beta and Beta-style programming

(define mmpn-book-1993
  (make-bib
   #:title "Object-Oriented Programming in the BETA Programming Language"
   #:author (authors "Ole Lehrmann Madsen" "Birger Møller-Pedersen"
                     "Kristen Nygaard")
   #:date 1993
   #:is-book? #true
   #:location (book-location #:publisher "Addison-Wesley Publishing Co.")))

(define gff-oopsla-2004
  (make-bib
   #:title "Super and Inner: Together at Last!"
   #:author (authors "David S. Goldberg" "Robert Bruce Findler"
                     "Matthew Flatt")
   #:date 2004
   #:location (proceedings-location oopsla #:pages '(116 129))))

;; ----------------------------------------
; Types for delim control

(define Danvy1989
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "A Functional Abstraction of Typed Contexts"
   #:location (techrpt-location #:institution "University of Copenhagen"
                                ;; I'm not 100% sure of the TR number since
                                ;; it's not listed anywhere officially
                                #:number "DIKU Report 89/12")
   #:date 1989))

(define AbstractingControl
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "Abstracting Control"
   #:location (proceedings-location lfp #:pages '(151 160))
   #:date 1990))

(define Gunter1995
  (make-bib
    #:author (authors "Carl A. Gunter" "Remy Didier" "Jon G. Riecke")
    #:title "A Generalization of Exceptions and Control in ML-like Languages"
    #:location (proceedings-location fpca #:pages '(12 23))
    #:date 1995))

(define Kiselyov2007
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan")
   #:title "A Substructural Type System for Delimited Continuations"
   #:location (proceedings-location tlca #:pages '(223 239))
   #:date 2007))

(define Asai2007
  (make-bib
    #:author (authors "Kenichi Asai" "Yukiyoshi Kameyama")
    #:title "Polymorphic Delimited Continuations"
    #:location (proceedings-location asplas #:pages '(239 254) #;#:series #;"LNCS 4807")
    #:date 2007))

(define Dybvig2007
  (make-bib
   #:author (authors "Kent Dybvig"
                     "Simon Peyton-Jones"
                     "Amr Sabry")
   #:title "A Monadic Framework for Delimited Continuations"
   #:location (journal-location jfp
                                #:volume 17
                                #:number 6
                                #:pages '(687 730))
   #:date 2007))

(define James2011
  (make-bib
   #:author (authors "Roshan P. James" "Amr Sabry")
   #:title "Yield: Mainstream Delimited Continuations"
   #:location (proceedings-location "Theory and Practice of Delimited Continuations" #:pages '(20 32))
   #:date 2011))

(define control-tr
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (techrpt-location #:institution "Northeastern University"
                                #:number "NU-CCIS-13-01")
   #:date "2013"))

;; ----------------------------------------
; Education

(define scriven-chapter-1967
  (make-bib
   #:author "Michael Scriven"
   #:title "The Methodology of Evaluation. Perspectives of Curriculum Evaluation"
   #:is-book? #true
   #:location (book-location #:publisher "Rand McNally")
   #:date 1967))

(define fcffksf-jfp-2002
  (make-bib
   #:author (authors "Robert Bruce Findler" "John Clements"
                     "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Paul Steckler"
                     "Matthias Felleisen")
   #:title "DrScheme: a Programming Environment for Scheme"
   #:location (journal-location jfp
                                #:volume 12
                                #:number 2
                                #:pages '(159 182))
   #:date 2002))

(define fffk-icfp-2009
  (make-bib
   #:author (authors "Matthias Felleisen" "Robert Bruce Findler"
                     "Matthew Flatt" "Shriram Krishnamurthi")
   #:title "A Functional I/O System (or Fun for Freshman Kids)"
   #:location (proceedings-location icfp #:pages '(47 58))
   #:date 2009))

;; ----------------------------------------
; Racket

(define ffkf-icfp-1999
  (make-bib
   #:author (authors "Matthew Flatt" "Rober Bruce Findler"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Programming Languages as Operating Systems (or Revenge of the Son of the Lisp Machine)"
   #:location (proceedings-location icfp #:pages '(138 147))
   #:date 1999))

(define fff-asplas-2006
  (make-bib
   #:author (authors "Matthew Flatt" "Robert Bruce Findler"
                     "Matthias Felleisen")
   #:title "Scheme with Classes, Mixins, and Traits"
   #:location (proceedings-location asplas #:pages '(270 289))
   #:date 2006))

(define fbf-icfp-2009
  (make-bib
   #:author (authors "Matthew Flatt" "Eli Barzilay"
                     "Robert Bruce Findler")
   #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
   #:location (proceedings-location icfp #:pages '(109 120))
   #:date 2009))

(define st-icfp-2013
  (make-bib
   #:author (authors "Vincent St-Amour" "Neil Toronto")
   #:title "Applying Random Testing to a Base Type Environment"
   #:location (proceedings-location icfp #:pages '(351 356))
   #:date 2013))

(define saf-cc-2015
  (make-bib
   #:author (authors "Vincent St-Amour" "Leif Andersen" "Matthias Felleisen")
   #:title "Feature-specific Profiling"
   #:location (proceedings-location cc #:pages '(49 68))
   #:date 2015))

(define stf-oopsla-2012
  (make-bib
    #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt" "Matthias Felleisen")
    #:title "Optimization coaching"
    #:location (proceedings-location oopsla #:pages '(163 178))
    #:date 2012))

;; ----------------------------------------
; Pycket

;(define fbpsth-dyla-2014
;  (make-bib
;   #:author (authors "Carl Friedrich Bolz" "Tobias Pape"
;                     "Jeremy G. Siek" "Sam Tobin-Hochstadt")
;   #:title "Meta-tracing makes a fast Racket"
;   #:location (proceedings-location dyla)
;   #:date 2014))

(define bauman-et-al-icfp-2015
  (make-bib
   #:author (authors "Spenser Bauman" "Carl Friedrich Bolz" "Robert Hirschfield"
                     "Vasily Kirilichev" "Tobias Pape" "Jeremy G. Siek"
                     "Sam Tobin-Hochstadt")
   #:title "Pycket: A Tracing JIT For a Functional Language"
   #:location (proceedings-location icfp #:pages '(22 34))
   #:date 2015))

;; ----------------------------------------
; Pluggable types

;(define b-ordl-2004
;  ;http://bracha.org/pluggableTypesPosition.pdf
;  (make-bib
;   #:author "Gilad Bracha"
;   #:title "Pluggable Type Systems"
;   #:location (proceedings-location "OOPSLA Workshop on Revival of Dynamic Languages")
;   #:date 2004))

(define pacpe-issta-2008
  (make-bib
   #:author (authors "Matthew M. Papi" "Mahmood Ali" "Telmo Louis Correa, Jr."
                     "Jeff H. Perkins" "Michael D. Ernst")
   #:title "Practical Pluggable Types for Java"
   #:location (proceedings-location issta #:pages '(201 212))
   #:date 2008))

; -----------------------------------------------------------------------------
; --- arxiv

;(define kas-arxiv-2018
;  (make-bib
;    #:author (authors "Andre Kuhlenschmidt" "Deyaaeldeen Almahallawi" "Jeremy G. Siek")
;    #:title "Efficient Gradual Typing"
;    #:location "arXiv:1802.06375v1"
;    #:date 2018))

;(define nl-arxiv-2018
;  (make-bib
;    #:author (authors "Max S. New" "Daniel R. Licata")
;    #:title "Call-by-name Gradual Type Theory"
;    #:location "arXiv:1802.00061"
;    #:date 2018))

; -----------------------------------------------------------------------------

(define m-maclisp-1974
  (make-bib
   #:author "David A. Moon"
   #:title "MACLISP Reference Manual"
   #:location (techrpt-location #:institution "MIT" #:number "Revision 0")
   #:date 1974))

(define hm-icfp-2004
  (make-bib
   #:author (authors "David Herman" "Philippe Meunier")
   #:title "Improving the Static Analysis of Embedded Languages via Partial Evaluation"
   #:location (proceedings-location icfp #:pages '(16 27))
   #:date 2004))

(define fi-jfp-2000
  (make-bib
   #:title "Do we need dependent types?"
   #:author "Daniel Friedlander and Mia Indrika"
   #:location (journal-location jfp
                                #:volume 10
                                #:number 4
                                #:pages '(409 415))
   #:date 2000))

(define lb-sigplan-2014
  (make-bib
   #:title "Hasochism: The Pleasure and Pain of Dependently Typed Programming"
   #:author "Sam Lindley and Conor McBride"
   #:location (proceedings-location sigplan-notices #:pages '(81 92))
   #:date 2014))

(define ddems-icse-2011
  (make-bib
   #:title "Building and Using Pluggable Type Checkers"
   #:author (authors "Werner Dietl" "Stephanie Dietzel" "Michael D. Ernst" "Kıvanç Muşlu" "Todd W. Schiller")
   #:location (proceedings-location icse #:pages '(681 690))
   #:date 2011))

(define a-icfp-1999
  (make-bib
   #:title "Cayenne --- a language with dependent types"
   #:author "Lennart Augustsson"
   #:location (proceedings-location icfp #:pages '(239 250))
   #:date 1998))

(define f-popl-2016
  (make-bib
   #:title "Bindings as Sets of Scopes"
   #:author "Matthew Flatt"
   #:location (proceedings-location popl #:pages '(705 717))
   #:date 2016))

(define c-jsl-1997
  (make-bib
   #:title "Three Uses of the Herbrand-Gentzen theorem in relating model theory and proof theory"
   #:author "William Craig"
   #:location (journal-location jsl
                                #:volume 22
                                #:number 3
                                #:pages '(269 285))
   #:date 1957))

(define wmpk-algol-1968
  (make-bib
   #:title "Report on the Algorithmic Language ALGOL 68"
   #:author (authors "A. van Wijngaarden" "B. J. Mailloux" "J.E.L. Peck" "C.H.A. Koster")
   #:date 1968))

(define s-lisp-1990
  (make-bib
   #:title "Common Lisp the Language"
   #:author "Guy L. Steele"
   #:location (book-location #:edition "2nd" #:publisher "Digital Press")
   #:is-book? #t
   #:date 1990))

(define mbb-sigmod-2006
  (make-bib
   #:title "LINQ: Reconciling Object, Relations and XML in the .NET Framework"
   #:author (authors "Erik Meijer" "Brain Beckman" "Gavin Bierman")
   #:location (proceedings-location sigmod #:pages '(706 706))
   #:date 2006))

(define c-dissertation-2010
  (make-bib
   #:title "Refining Syntactic Sugar: Tools for Supporting Macro Development"
   #:author "Ryan Culpepper"
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2010))

(define f-icfp-2002
  (make-bib
   #:title "Composable and Compilable Macros: You Want it When?"
   #:author "Matthew Flatt"
   #:location (proceedings-location icfp #:pages '(72 83))
   #:date 2002))

(define ew-haskell-2012
  (make-bib
   #:title "Dependently Typed Programming with Singletons"
   #:author "Richard A. Eisenberg and Stephanie Weirich"
   #:location (proceedings-location haskell #:pages '(117 130))
   #:date 2012))

(define ks-plpv-2006
  (make-bib
   #:title "Lightweight static capabilities"
   #:author "Oleg Kiselyov and Chung-chieh Shan"
   #:location (proceedings-location plpv #:pages #false)
   #:date 2006))

(define b-scala-2013
  (make-bib
   #:title "Scala Macros: Let our Powers Combine!"
   #:author "Eugene Burmako"
   #:location (proceedings-location scala #:pages #false)
   #:date 2013))

(define ro-gpce-2010
  (make-bib
   #:title "Lightweight Modular Staging: A Pragmatic Approach to Runtime Code Generation and Compiled DSLs"
   #:author (authors "Tiark Rompf" "Martin Odersky")
   #:location (proceedings-location gpce #:pages #false)
   #:date 2010))

(define kkt-pldi-2016
  (make-bib
   #:title "Occurrence Typing Modulo Theories"
   #:author (authors "Andrew Kent" "David Kempe" "Sam Tobin-Hochstadt")
   #:location (proceedings-location pldi #:pages '(296 309))
   #:date 2016))

(define fcdb-jfp-2012
  (make-bib
   #:title "Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts"
   #:author (authors "Matthew Flatt" "Ryan Culpepper" "David Darais" "Robert Bruce Findler")
   #:location (proceedings-location jfp #:pages '(181 216))
   #:date 2012))

(define ramho-hosc-2013
  (make-bib
   #:title "Scala-Virtualized: Linguistic Reuse for Deep Embeddings"
   #:author (authors "Tiark Rompf" "Nada Amin" "Adriaan Moors" "Philipp Haller" "Martin Odersky")
   #:location (journal-location hosc #:volume 25 #:number 1 #:pages '(165 207))
   #:date 2012))

(define stf-esop-2009
  (make-bib
   #:title "Practical Variable-Arity Polymorphism"
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(32 46))
   #:date 2009))

(define ra-icfp-2015
  (make-bib
   #:title "Functional Pearl: A SQL to C compiler in 500 Lines of Code"
   #:author (authors "Tiark Rompf" "Nada Amin")
   #:location (proceedings-location icfp #:pages '(2 9))
   #:date 2015))

(define lm-dsl-1999
  (make-bib
   #:title "Domain Specific Embedded Compilers"
   #:author (authors "Daan Leijen" "Erik Meijer")
   #:location (proceedings-location dsl #:pages '(109 122))
   #:date 1999))

(define cf-icfp-2010
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(235 246))
   #:date 2010))

(define pss-tacas-1998
  (make-bib
   #:title "Translation Validation"
   #:author (authors "Amir Pnueli" "Michael Siegel" "Eli Singerman")
   #:location (proceedings-location tacas #:pages '(151 166))
   #:date 1998))

(define le-popl-2016
  (make-bib
    #:title "Sound Type-Dependent Syntactic Language Extension"
    #:author (authors "Florian Lorenzen" "Sebastian Erdweg")
    #:location (proceedings-location popl #:pages '(204 216))
    #:date 2016))

(define gr-cup-2004
  (make-bib
    #:title "The Standard ML Base Library"
    #:author (authors "Emden R. Gansner" "John H. Reppy")
    #:is-book? #true
    #:location (book-location #:edition "1st" #:publisher "Cambridge University Press")
    #:date 2004))

(define wd-sas-1997
  (make-bib
    #:title "Fast and Effective Procedure Inlining"
    #:author (authors "Oscar Waddell" "R. Kent Dybvig")
    #:location (proceedings-location sas #:pages '(35 52))
    #:date 1997))

(define ghc-typechecker-plugin
  (make-bib
   #:title "Type Checker Plugins"
   #:url "https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker"
   #:note "Accessed 2016-06-30."
   #:date 2015))

(define b-plugin
  (make-bib
   #:title "GHC type checker plugins: adding new type-level operations"
   #:author "Christiaan Baaij"
   #:url "http://christiaanb.github.io/posts/type-checker-plugin/"
   #:note "Accessed 2016-06-30."
   #:date 2016))

(define d-haskell-2015
  (make-bib
   #:title "Improving Haskell Types with SMT"
   #:author "Iavor Diatchki"
   #:location (proceedings-location haskells #:pages '(1 10))
   #:date 2015))

(define g-haskell-2015
  (make-bib
   #:title "A Typechecker Plugin for Units of Measure: Domain-Specific Constraint Solving in GHC"
   #:author "Adam Gundry"
   #:location (proceedings-location haskells #:pages '(11 22))
   #:date 2015))

(define r-compiler-plugins
  (make-bib
   #:title "Compiler Plugins"
   #:url "https://doc.rust-lang.org/book/compiler-plugins.html"
   #:date 2016
   #:note "Accessed 2016-06-30."))

;; -- macros overview
; http://www.ncameron.org/blog/macros-in-rust-pt2/

;; -- types of syntax extensions
; https://dxr.mozilla.org/rust/source/src/libsyntax/ext/base.rs

;; -- rust regexp types
; https://doc.rust-lang.org/regex/regex/struct.Regex.html

(define k-io
  (make-bib
   #:title "Type-safe functional formatted IO"
   #:author "Oleg Kiselyov"
   #:url "http://okmij.org/ftp/typed-formatting/"
   #:date 2014
   #:note "Accessed 2016-06-30."))

;; Coq overhead < 2x
(define mztg-pldi-2016
  (make-bib
   #:title "Verified Peephole Optimizations for CompCert"
   #:author (authors "Eric Mullen" "Daryl Zuniga" "Zachary Tatlock" "Dan Grossman")
   #:location (proceedings-location pldi #:pages '(448 461))
   #:date 2016))

;; Coq overhead 300x
(define ckmr-pldi-2012
  (make-bib
   #:title "Proving Acceptability Properties of Relaxed Nondeterministic Approximate Programs"
   #:author (authors "Michael Carbin" "Deokhwan Kim" "Sasa Misailovic" "Martin C. Rinard")
   #:location (proceedings-location pldi #:pages '(169 180))
   #:date 2012))

(define fs-jfp-2008
  (make-bib
   #:title "Building Language Towers with Ziggurat"
   #:author (authors "David Fisher" "Olin Shivers")
   #:location (journal-location jfp
                                #:volume 18
                                #:number 6
                                #:pages '(707 780))
   #:date 2008))

(define pk-icfp-2015
  (make-bib
    #:title "Hygenic Resugaring of Compositional Desugaring"
    #:author (authors "Justin Pombrio" "Shriram Krishnamurthi")
    #:location (proceedings-location icfp #:pages '(75 87))
    #:date 2015))


(define growing-a-language
  (make-bib
    #:title "Growing a Language"
    #:author "Guy L. Steele, Jr."
    #:location (proceedings-location oopsla #:pages '("0.01" "A1"))
    #:date 1998))

(define h-acm-1996
  (make-bib
    #:title "Building Domain-Specific Embedded Languages"
    #:author "Paul Hudak"
    #:location (journal-location "ACM Computing Surveys"
                                 #:volume 28
                                 #:number "4es")
    #:date 1996))

(define hm-toplas-1993
  (make-bib
   #:title "On the Type Structure of Standard ML"
   #:author (authors "Robert Harper" "John C. Mitchell")
   #:location (journal-location toplas
                                #:volume 15
                                #:number "2"
                                #:pages '(211 252))
   #:date 1993))

(define cgk-popl-2017
  (make-bib
   #:title "Type Systems as Macros"
   #:author (authors "Stephen Chang" "Alex Knauth" "Ben Greenman")
   #:location (proceedings-location popl #:pages '(694 705))
   #:date 2017))

(define cks-jfp-2009
  (make-bib
   #:title "Finally Tagless, Partially Evaluated"
   #:author (authors "Jacques Carette" "Oleg Kiselyov" "Chung-chieh Shan")
   #:location (proceedings-location jfp #:pages '(509 543))
   #:date 2009))

(define cc-popl-1977
  (make-bib
   #:title "Abstract Interpretation: A Unified Lattice Model for Static Analysis of Programs by Construction or Approximation of Fixpoints"
   #:author (authors "Patrick Cousot" "Radhia Cousot")
   #:location (proceedings-location popl #:pages '(238 252))
   #:date 1977))

(define bat-ecoop-2014
  (make-bib
    #:title "Understanding TypeScript"
    #:author (authors "Gavin Bierman" "Martin Abadi" "Mads Torgersen")
    #:location (proceedings-location ecoop #:pages '(257 281))
    ;#:url "http://dx.doi.org/10.1007/978-3-662-44202-9_11"
    #:date 2014))

(define vss-popl-2017
  (make-bib
    #:title "Big Types in Little Runtime: Open-World Soundness and Collaborative Blame for Gradual Type Systems"
    #:author (authors "Michael M. Vitousek" "Cameron Swords" "Jeremy G. Siek")
    #:location (proceedings-location popl #:pages '(762 774))
    #:date 2017))

(define ar-lp-1994
  (make-bib
    #:title "Performing Lisp analysis of the FANNKUCH benchmark"
    #:author (authors "Kenneth R. Anderson" "Duane Rettig")
    #:location (journal-location "ACM SIGPLAN Lisp Pointers"
                                 #:volume 7
                                 #:number 4
                                 #:pages '(2 12))
    #:date 1994))

(define n-mthesis-2014
  (make-bib
   #:title "Tough Behavior in Repeated Bargaining game, A Computer Simulation Study"
   #:author "Linh Chi Nguyen"
   #:location (dissertation-location #:institution "University of Trento" #:degree "Master in Economics")
   #:date 2014))

(define svcb-snapl-2015
  (make-bib
    #:title "Refined Criteria for Gradual Typing"
    #:author (authors "Jeremy G. Siek" "Michael M. Vitousek" "Matteo Cimini" "John Tang Boyland")
    #:location (proceedings-location snapl #:pages '(274 293))
    #:date 2015))

(define tfffgksst-snapl-2017
  (make-bib
   #:title "Migratory Typing: Ten years later"
   #:author (authors "Sam Tobin-Hochstadt"
                     "Matthias Felleisen"
                     "Robert Bruce Findler"
                     "Matthew Flatt"
                     "Ben Greenman"
                     "Andrew M. Kent"
                     "Vincent St-Amour"
                     "T. Stephen Strickland"
                     "Asumu Takikawa")
   #:location (proceedings-location snapl #:pages '(17:1 17:17))
   ;; http://drops.dagstuhl.de/opus/volltexte/2017/7120/
   #:date 2017))

(define ff-cjm-1956
  (make-bib
    #:title "Maximal flow through a network"
    #:author (authors "L. R. Ford" "D. R. Fulkerson")
    #:location (journal-location "Canadian Journal of Mathematics"
                 #:volume 8
                 #:pages '(399 404))
    #:date 1956))

(define k-ams-1956
  (make-bib
    #:title "On the shortest spanning subtree of a graph and the traveling salesman problem"
    #:author "Joseph B. Kruskal, Jr."
    #:location (journal-location "Proceedings of the American Mathematical Society"
                 #:volume 7
                 #:number 1
                 #:pages '(48 50))
    #:date 1956))

(define k-cs-1974
  (make-bib
    #:title "Structured programming with go to statements"
    #:author "Donald E. Knuth"
    #:location (journal-location "Computing Surveys"
                                 #:volume 6
                                 #:number 4
                                 #:pages '(261 301))
    #:date 1974))

(define mdhs-asplos-2009
  (make-bib
    #:title "Producing Wrong Data Without Doing Anything Obviously Wrong!"
    #:author (authors "Todd Mytkowicz" "Amer Diwan" "Matthias Hauswirth" "Peter F. Sweeney")
    #:location (proceedings-location asplos #:pages '(265 276))
    #:date 2009))

(define vs-tr-2016
  (make-bib
   #:title "Gradual typing in an open world"
   #:author (authors "Michael M. Vitousek" "Jeremy Siek")
   #:location (techrpt-location #:institution "Indiana University" #:number "729")
   #:date 2016))

(define ssm-esop-2014
  (make-bib
    #:title "An Array-Oriented Language with Static Rank Polymorphism"
    #:author (authors "Justin R. Slepak" "Olin Shivers" "Panagiotis Manolios")
    #:location (proceedings-location esop #:pages '(27 46))
    #:date 2014))

(define s-thesis-2015
  (make-bib
    #:title "How to generate actionable advice about performance problems"
    #:author "Vincent St-Amour"
    #:location (dissertation-location #:institution "Northeastern University"
                                      #:degree "Ph.D.")
    #:date 2015))

(define f-keynote-2002
  (make-bib
    #:title "From POPL to the classroom and back"
    #:author "Matthias Felleisen"
    #:location (proceedings-location popl #:pages '(126 127))
    #:date 2002))

(define wmwz-ecoop-2017
  (make-bib
    #:title "Mixed Messages: Measuring Conformance and Non-Interference in TypeScript"
    #:author (authors "Jack Williams" "J. Garrett Morris" "Philip Wadler" "Jakub Zalewski")
    #:location (proceedings-location ecoop #:pages '(28:1 28:29))
    #:date 2017))

(define gm-pepm-2018
  (make-bib
    #:title "On the Cost of Type-Tag Soundness"
    #:author (authors "Ben Greenman" "Zeina Migeed")
    #:location (proceedings-location pepm #:pages '(30 39))
    #:date 2018))

(define r-ip-1983
  (make-bib
   #:title "Types, Abstraction, and Parametric Polymorphism"
   #:author "John C. Reynolds"
   #:location (proceedings-location ip #:pages '(513 523))
   #:date 1983))

(define pqk-onward-2012
  (make-bib
   #:title "Progressive Types"
   #:author (authors "Joe Gibbs Politz" "Hannah Quay-de la Vallee" "Shriram Krishnamurthi")
   #:location (proceedings-location onward #:pages '(55 66))
   #:date 2012))

(define bbst-oopsla-2017
  (make-bib
   #:title "Sound Gradual Typing: Only Mostly Dead"
   #:author (authors "Spenser Bauman" "Carl Friedrich Bolz-Tereick" "Jeremy Siek" "Sam Tobin-Hochstadt")
   #:location (journal-location pacmpl #:volume "1" #:number "OOPSLA" #:pages '("54:1" "54:24"))
   #:date 2017))

(define rmhn-ecoop-2019
  (make-bib
    #:author (authors "Richard Roberts" "Stefan Marr" "Michael Homer" "James Noble")
    #:title "Transient Typechecks are (Almost) Free"
    #:location (proceedings-location ecoop #:pages '(15:1 15:29))
    #:date 2019))

(define mt-oopsla-2017
  (make-bib
   #:title "Sound Gradual Typing is Nominally Alive and Well"
   #:author (authors "Fabian Muehlboeck" "Ross Tate")
   #:location (journal-location pacmpl #:volume "1" #:number "OOPSLA" #:pages '("56:1" "56:30"))
   #:date 2017))

(define cvgrl-oopsla-2017
  (make-bib
    #:author (authors "Avik Chaudhuri" "Panagiotis Vekris" "Sam Goldman" "Marshall Roch" "Gabriel Levy")
    #:title "Fast and Precise Type Checking for JavaScript"
    #:location (journal-location pacmpl #:volume "1" #:number "OOPSLA" #:pages '(48:1 48:30))
    #:date 2017))

(define vsc-dls-2019
  (make-bib
    #:author (authors "Michael M. Vitousek" "Jeremy G. Siek" "Avik Chaudhuri")
    #:title "Optimizing and Evaluating Transient Gradual Typing"
    #:location (proceedings-location dls #:pages '(28 41))
    #:date 2019))

(define gfd-oopsla-2019
  (make-bib
    #:title "Complete Monitors for Gradual Types"
    #:author (authors "Ben Greenman" "Matthias Felleisen" "Christos Dimoulas")
    #:location (journal-location pacmpl #:volume "3" #:number "OOPSLA" #:pages '("122:1" "122:29"))
    #:date "2019"))
(define g-popl-2015
  (make-bib
   #:title "Space-Efficient Manifest Contracts"
   #:author "Michael Greenberg"
   #:location (proceedings-location popl #:pages '(181 194))
   #:date 2015))

(define bc-sblp-2009
  (make-bib
   #:title "Function Inheritance: Monadic Memoization Mixins"
   #:author (authors "Daniel Brown" "William R. Cook")
   #:location (proceedings-location sblp #:pages #false)
   #:date 2009))

(define fgr-ifl-2007
  (make-bib
   #:title "Lazy Contract Checking for Immutable Data Structures"
   #:author (authors "Robert Bruce Findler" "Shu-yu Guo" "Anne Rogers")
   #:location (proceedings-location ifl #:pages '(111 128))
   #:date 2007))

(define w-jcss-1979
  (make-bib
   #:title "Final Algebra Semantics and Data Type Extension"
   #:author (authors "Mitchell Wand")
   #:location (journal-location jcss #:volume 19 #:pages '(27 44))
   #:date 1979))

(define bbdt-ecoop-2016
  (make-bib
   #:title "Fine-grained Language Composition: A Case Study"
   #:author (authors "Edd Barrett" "Carl Friedrich Bolz" "Lukas Diekmann" "Laurence Tratt")
   #:location (proceedings-location ecoop #:pages '(3:1 3:27))
   #:date 2016))

(define gff-oopsla-2005
  (make-bib
   #:title "Fine-Grained Interoperability Through Mirrors and Contracts"
   #:author (authors "Kathryn E. Gray" "Robert Bruce Findler" "Matthew Flatt")
   #:location (proceedings-location oopsla #:pages '(231 245))
   #:date 2005))

(define ks-mscs-2017
  (make-bib
   #:title "Practical Coinduction"
   #:author (authors "Dexter Kozen" "Alexandra Silva")
   #:location (journal-location mscs #:volume 27 #:pages '(1132 1152))
   #:date 2017))

(define b-lambda-1981
  (make-bib
   #:title "The Lambda Calculus: Its Syntax and Semantics"
   #:author "Henk Barendregt"
   #:is-book? #true
   #:location (book-location #:publisher "North-Holland Publishing Company")
   #:date 1981))

(define c-lp-1983
  (make-bib
   #:title "Mathematics as Programming"
   #:author "Robert L. Constable"
   #:location (proceedings-location lp #:pages '(116 128))
   #:date 1983))

(define rat-oopsla-2017
  (make-bib
    #:title "The VM Already Knew That: Leveraging Compile-Time Knowledge to Optimize Gradual Typing"
    #:author (authors "Gregor Richards" "Ellen Arteca" "Alexi Turcotte")
    #:location (journal-location pacmpl #:volume "1" #:number "OOPSLA" #:pages '("55:1" "55:27"))
    #:date 2017))

(define cf-ecoop-2016
  (make-bib
    #:title "Interprocedural Type Specialization of JavaScript Programs Without Type Analysis"
    #:author (authors "Maxime Chevalier-Boisvert" "Marc Feeley")
    #:location (proceedings-location ecoop #:pages '(7:1 7:24))
    #:date 2016))

(define dtw-pepm-2012
  (make-bib
    #:title "The Interaction of Contracts and Laziness"
    #:author (authors "Markus Degen" "Peter Thiemann" "Stefan Wehr")
    #:location (proceedings-location pepm #:pages '(97 106))
    #:date 2012))

(define a-cc-1992
  (make-bib
    #:title "Compiling with Continuations"
    #:author "Andrew W. Appel"
    #:location (book-location #:edition "1st" #:publisher "Cambridge University Press")
    #:date 1992))

(define s-cl-1990
  (make-bib
    #:title "Common Lisp the Language"
    #:author (authors "Guy L. Steele")
    #:location (book-location #:edition "2nd" #:publisher "Digital Press")
    #:date 1990))

(define gct-popl-2016
  (make-bib
    #:title "Abstracting Gradual Typing"
    #:author (authors "Ronald Garcia" "Alison M. Clark" "Éric Tanter")
    #:location (proceedings-location popl #:pages '(429 442))
    #:date 2016))

(define g-tfp-2016
  (make-bib
   #:author "Michael Greenberg"
   #:title "Space-Efficient Latent Contracts"
   #:location (proceedings-location tfp #:pages #false)
   #:date 2016))

(define gf-tr-2018
  (make-bib
    #:author (authors "Ben Greenman" "Matthias Felleisen")
    #:title "A Spectrum of Type Soundness and Performance: Supplementary Material"
    #:location (techrpt-location #:institution "Northeastern University" #:number "NU-CCIS-2018-002")
    #:date 2018))

(define agd-ecoop-2005
  (make-bib
    #:author (authors "Christopher Anderson" "Paola Giannini" "Sophia Drossopoulou")
    #:title "Towards Type Inference for JavaScript"
    #:location (proceedings-location ecoop #:pages '(428 452))
    #:date 2005))

(define nl-fscd-2018
  (make-bib
    #:author (authors "Max S. New" "Daniel R. Licata")
    #:title "Call-by-name Gradual Type Theory"
    #:location (proceedings-location fscd #:pages #false)
    #:date 2018))

(define hms-dls-2016
  (make-bib
    #:author (authors "Thomas S. Heinze" "Anders Møller" "Fabio Strucco")
    #:title "Type Safety Analysis for Dart"
    #:location (proceedings-location dls #:pages '(1 12))
    #:date 2016))

;(define m-ilc-2010
;  (make-bib
;    #:author (authors "Hannes Mehnert")
;    #:title "Extending Dylan's Type System for better type inference and error detection"
;    #:location (proceedings-location ilc #:pages '(1 10))
;    #:date 2010))

(define r-jfp-2008
  (make-bib
    #:author (authors "Norman Ramsey")
    #:title "Embedding an interpreted language using higher-order functions and types"
    #:location (journal-location jfp #:volume 21 #:number 6 #:pages '(585 615))
    #:date 2008))

(define ok-popl-2003
  (make-bib
    #:author (authors "Atsushi Ohori" "Kazuhiko Kato")
    #:title "Semantics for Communication Primitives in a Polymorphic Language"
    #:location (proceedings-location popl #:pages '(99 112))
    #:date 1993))

(define fb-flops-2006
  (make-bib
    #:author (authors "Robert Bruce Findler" "Matthias Blume")
    #:title "Contracts as Pairs of Projections"
    #:location (proceedings-location flops #:pages '(226 241))
    #:date 2006))

(define h-lfp-1994
  (make-bib
    #:author "Nevin Heintze"
    #:title "Set-based analysis of ML-programs"
    #:location (proceedings-location lfp #:pages '(306 317))
    #:date 1994))

(define mfsw-hosc-2005
  (make-bib
    #:author (authors "Philippe Meunier" "Robert Bruce Findler" "Paul Steckler" "Mitchell Wand")
    #:title "Selectors Make Set-Based Analysis Too Hard"
    #:location (journal-location hosc #:volume 18 #:pages '(245 269))
    #:date 2005))

(define clzv-ecoop-2018
  (make-bib
    #:author (authors "Benjamin W. Chung" "Paley Li" "Francesco Zappa Nardelli" "Jan Vitek")
    #:title "KafKa: Gradual Typing for Objects"
    #:location (proceedings-location ecoop #:pages '(12:1 12:23))
    #:date 2018))

(define ajsw-icfp-2017
  (make-bib
    #:author (authors "Amal Ahmed" "Dustin Jamner" "Jeremy G. Siek" "Philip Wadler")
    #:title "Theorems for Free for Free: Parametricity, With and Without Types"
    #:location (journal-location pacmpl #:volume 1 #:number "ICFP" #:pages '(39:1 39:28))
    #:date 2017))

(define isi-icfp-2017
  (make-bib
    #:author (authors "Yuu Igarashi" "Taro Sekiyama" "Atsushi Igarashi")
    #:title "On Polymorphic Gradual Typing"
    #:location (journal-location pacmpl #:volume "1" #:number "ICFP" #:pages '("40:1" "40:29"))
    #:date 2017))

(define stw-pldi-2015
  (make-bib
    #:author (authors "Jeremy Siek" "Peter Thiemann" "Philip Wadler")
    #:title "Blame and Coercion: Together Again for the First Time"
    #:location (proceedings-location pldi #:pages '(425 435))
    #:date 2015))

(define p-tcs-1977
  (make-bib
    #:author "G.D. Plotkin"
    #:title "LCF Considered as a Programming Language"
    #:location (journal-location tcs #:volume 5 #:pages '(223 255))
    #:date 1977))

(define pmw-dls-2009
  (make-bib
    #:author (authors "Frédéric Pluquet" "Antoine Marot" "Roel Wuyts")
    #:title "Fast Type Reconstruction for Dynamically Typed Programming Languages"
    #:location (proceedings-location dls #:pages '(69 78))
    #:date 2009))

(define cs-popl-2016
  (make-bib
    #:author (authors "Matteo Cimini" "Jeremy G. Siek")
    #:title "The Gradualizer: A Methodology and Algorithm for Generating Gradual Type Systems"
    #:location (proceedings-location popl #:pages '(443 455))
    #:date 2016))

(define lt-popl-2017
  (make-bib
    #:author (authors "Nico Lehmann" "Éric Tanter")
    #:title "Gradual Refinement Types"
    #:location (proceedings-location popl #:pages '(775 788))
    #:date 2017))

(define b-types-1995
  (make-bib
    #:author "Gilles Barthe"
    #:title "Implicit Coercions in Type Systems"
    #:location (proceedings-location types #:pages '(1 15))
    #:date 1995))

(define g-tr-2016
  (make-bib
    #:author (authors "Andrew P. Black" "Kim B. Bruce" "James Noble")
    #:title "The Grace Programming Language Draft Specification Version 0.7.0"
    #:location "http://gracelang.org/documents/grace-spec-0.7.0.pdf accessed 2018-05-25"
    #:date 2016))

(define lybb-tr-2011
  (make-bib
    #:author (authors "Tim Lindholm" "Frank Yellin" "Gilad Bracha" "Alex Buckley")
    #:title "The Java® Virtual Machine Specification"
    #:location (techrpt-location #:institution "JSR-000924" #:number "7")
    #:date 2011))

(define m-jcss-1978
  (make-bib
    #:author "Robin Milner"
    #:title "A Theory of Type Polymorphism in Programming"
    #:location (journal-location jcss #:pages '(348 375) #:number 3 #:volume 17)
    #:date 1978))

(define fgsfs-oopsla-2018
  (make-bib
    #:author (authors "Daniel Feltey" "Ben Greenman" "Christophe Scholliers" "Robert Bruce Findler" "Vincent St-Amour")
    #:title "Collapsible Contracts: Fixing a Pathology of Gradual Typing"
    #:location (journal-location pacmpl #:pages '("133:1" "133:27") #:number oopsla #:volume 2)
    #:date 2018))

(define gi-scp-2020
  (make-bib
    #:author (authors "Hugo Musso Gualandi" "Roberto Ierusalimschy")
    #:title "Pallene: a companion language for Lua"
    #:location (journal-location scp #:pages '(1 15) #:volume 189 #:number 102393)
    #:date 2020))

(define tgpk-dls-2018
  (make-bib
    #:author (authors "Preston Tunnell Wilson" "Ben Greenman" "Justin Pombrio" "Shriram Krishnamurthi")
    #:title "The Behavior of Gradual Types: a User Study"
    #:location (proceedings-location dls #:pages '(1 12))
    #:date 2018))

(define clps-popl-2019
  (make-bib
    #:author (authors "Guiseppe Castagna" "Victor Lanvin" "Tommaso Petrucciani" "Jeremy G. Siek")
    #:title "Gradual Typing: A New Perspective"
    #:location (journal-location pacmpl #:volume 3 #:number popl #:pages '("16:1" "16:32"))
    #:date 2019))



