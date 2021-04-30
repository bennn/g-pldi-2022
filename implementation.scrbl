#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in "pict.rkt"
     fig:any-wrap
     fig:no-wrap
     fig:index-of
     untyped-codeblock
     typed-codeblock))

@; THESIS the TR compiler can support shallow
@; - outside-facing API : new lang for users, easy switch
@; - inside-facing API : reuse ~80% of compiler
@; - note typed libraries still deep
@; - note awkardness at typed-untyped utils
@;
@; - for technical details, point to PR or dissertation

@title[#:tag "sec:implementation"]{Implementation}
@; TODO cite experience report

@; 2021-04-29 do we really need to discuss typed/untyped API? could point to dissertation for details

The Typed Racket compiler can incorporate Shallow types.

@; X typed-context? hook = easy
@; X reuse ctc + type = hard
@; x #%module-begin, to reuse contract defs save space ... NVM
@; X require/untyped-contract
@; X define-typed/untyped-id
@; X reuse G macros in S code
@; - manual type env needs manual trust

The implementation of @|sShallow| Racket begins with two new @tt{#lang}
 languages to communicate the options available to programmers.
@itemlist[
@item{
  Modules that start with @tt{#lang typed/racket} continue to use @|sdeep| types,
   same as earlier versions of Typed Racket;
}
@item{
  @tt{#lang typed/racket/deep} is a new way to opt-in to @|sdeep| types;
}
@item{
  and @tt{#lang typed/racket/shallow} provides @|sshallow| types.
}]
@|noindent|All three languages invoke the same type checker.
At steps where @|sdeep| and @|sshallow| disagree,
 the compiler queries the current language to proceed.
For example, the type-directed optimizer checks that it has @|sdeep| types
 before rewriting code based on the @|sdeep| soundness guarantee.
@; Such queries are made possible by a module-level variable.

Many parts of the modified compiler use a similar, one-or-the-other strategy
 to handle @|sdeep| and @|sshallow| types.
This section deals with the more challenging aspects.
Sharing variables between @|sdeep| and @|sshallow| required changes to
 type-lookup and wrapper generation (@sectionref{sec:implementation:impl:code}).
Sharing macros requires further changes; currently, @|sdeep|-typed syntax can only
 be re-used through unsafe mechanisms (@sectionref{sec:implementation:impl:code}).
Lastly, Typed Racket has a small API that gives programmers control
 over the @|sdeep| type enforcement strategy.
This API needed generalizations to handle @|sshallow| types (@sectionref{sec:implementation:impl:tu}).


@section[#:tag "sec:implementation:impl:code"]{@|sDeep| and @|sShallow| Interaction}

Racket supports both separate compilation and hygienic macros@~cite{f-icfp-2002}.
Each module in a program gets compiled to a core language individually, and
 other modules can re-use the output.
Typed Racket cooperates with the separate compilation protocol by serializing
 the results of type checking@~cite{tscff-pldi-2011,ctf-sfp-2007}.
A well-typed module compiles to untyped code (with appropriate contracts)
 and a local type environment.
When one @|sdeep| module imports from another, it can find the type of the
 imported identifier in the type environment.

At first glance, it appears that @|sshallow| code can use the same protocol
 to find the type of @|sdeep| imports.
The protocol fails, however, because wrappers get in the way.
When @|sdeep| wants to provide an identifier, it really provides a piece of
 syntax called a rename transformer.
These transformers expand to one of two identifiers depending on where they
 appear: @|sdeep|-typed code gets the original identifier and can easily
 look up its type, but @|suntyped| and @|sshallow| code gets a wrapped
 version.
The wrapper causes a direct type lookup to fail.

For @|sdeep|-to-@|sshallow| exports, the solution is to modify
 type lookup to pass through wrappers.
Fortunately, the change was easy to make because the Racket contract library
 provides enough metadata.
At compile time (and only then), a wrapped identifier is associated with
 a structure that links back to the original.
The @|sshallow| type checker looks out for these wrappers and uncovers
 the originals as needed.

@|sShallow|-to-@|sdeep| exports use a dual method.
Like @|sdeep|, a @|sshallow| module provides only rename transformers.
These expand to the original identifier in other @|sshallow| and @|suntyped|
 code; the original is associated with type information.
For @|sdeep| clients, the transformers expand to a wrapped identifier.
Consequently, the @|sdeep| type checker watches for ``untyped'' wrappers and
 tests whether there is an available type.
Such types allow static type checks to succeed, and at run-time the wrapper
 keeps @|sdeep| code safe.

A surprising consequence of the final protocol is that a @|sshallow| module must
 be prepared to create wrappers for its exports.
The wrapper-making code is generated during compilation, at the end of
 type checking, but it does not run until needed by a @|sdeep| client.
In this way, only programs that depend on @|sdeep| code suffer from the expressiveness
 limits of wrappers.


@section[#:tag "sec:implementation:impl:syntax"]{Syntax Re-Use}

@|sShallow| code cannot use @|sdeep| macros.
Re-use is desirable to avoid copying code, but it requires a static analysis
 to enforce soundness.
This section explains the problem and criteria for a solution.

To appreciate the problem, consider the following simple macro.
This macro applies a typed function @tt{f} to an input, and is consequently
 unsafe:

@typed-codeblock['("(define-syntax-rule (call-f x) (f x))")]

@|noindent|If this macro could appear in @|sshallow| code, then any
 @|sshallow| value @tt{x} could sneak into the @|sdeep| function.
Unless @tt{f} makes no assumptions about its input, such values can break
 the @|sdeep| soundness guarantee and lead to dangerous results in optimized
 code.

One possible fix is to put a contract around every @|sdeep| identifier that
 appears in a macro.
Doing so would require an analysis to find out which contracts are needed,
 and a second analysis to install contracts wisely;
 each identifier requires a contract, but repeated occurrences of one identifier
 should not lead to repeated contract checks.
It should also be possible to avoid the contracts if the macro goes only to @|sdeep| clients.
These are major changes.

Another possibility is to statically check whether a macro is safe to
 export.
Safe macros appear, for example, in the typed compatibility layer for the
 RackUnit testing library.
RackUnit is an untyped library that exports some functions and some macros.
The typed layer provides types for the functions and type-annotated copies
 of the macros (about 300 lines in total).
These macros are safe because they do not expose any @|sdeep|-typed identifiers.
For example, the following macro combines a sequence of expressions into
 a named RackUnit test case:

@typed-codeblock['(
  "(define-syntax (test-case stx)"
  "  (syntax-parse stx"
  "    [(_ name expr ...)"
  "     (quasisyntax/loc stx"
  "       (parameterize ([test-name (ensure-str name)])"
  "         (test-begin expr ...)))]))"
)]

@|noindent|This macro is safe for @|sshallow| code, but for complicated reasons.
First, @tt{ensure-str} is a typed function that accepts any input.
Second, @tt{test-begin} is a macro from the same file that is also safe.
Third, @tt{parameterize} comes from untyped Racket.

@; ;; rackunit/rackunit-typed/rackunit/main.rkt
@;(define-syntax (test-begin stx)
@;  (syntax-case stx ()
@;    [(_ expr ...)
@;     (syntax/loc stx
@;       ((current-test-case-around)
@;        (lambda ()
@;          (with-handlers ([(λ (e)
@;                             (and (exn:fail? e)
@;                                  (not (exn:test? e))))
@;                           (λ ([e : exn:fail])
@;                             (test-log! #f)
@;                             (raise e))])
@;          (parameterize ([current-check-handler raise])
@;            (void)
@;            expr ...)))))]
@;    [_
@;     (raise-syntax-error
@;      #f
@;      "Correct form is (test-begin expr ...)"
@;      stx)]))

Currently, the author of a @|sdeep| library can enable syntax re-use by disabling the optimizer
 and unsafely providing macros.
This work-around requires a manual inspection, but it is more appealing than
 forking the RackUnit library and asking programmers to choose the correct version.


@section[#:tag "sec:implementation:impl:tu"]{@|sDeep|--@|sUntyped| Utilities}
@; struggles = edit old code , 2 of these => new type errors

Typed Racket has a small API by Neil Toronto to let programmers control boundaries
 between @|sdeep| and @|suntyped| code.
The API arose over time, as programmers (including Neil) discovered challenges.
Two forms in this API can lead to surprising results due to the existence of
 @|sshallow| code.

@; require/untyped-contract

The first problem concerns @tt{require/untyped-contract}.
This form lets untyped code import a typed identifier whose precise type
 cannot be expressed with a @|sdeep| contract.
Users supply a supertype of the precise type and @|sDeep| Racket uses this
 weaker type to generate a contract.

For example, the @bm{jpeg} benchmark depends on a library for multi-dimensional
 arrays (@render-lib[(make-lib "math/array" "https://docs.racket-lang.org/math/array.html")]).
This library accepts two kinds of data for array indices:
 either a vector of natural numbers or a vector of integers.
Helper functions assert that values with the integer type do not actually
 contain negative numbers using a run-time checking function:

@exact{\smallskip}
@typed-codeblock['(
  "(: check-array-shape"
  "   (-> (U (Vectorof Natural) (Vectorof Integer))"
  "       (Vectorof Natural)))")]

@|noindent|@|sDeep| contracts cannot express the type for the checking
 function because they lack support for true unions.
The work around is to impose a supertype on untyped clients:

@exact{\smallskip}
@untyped-codeblock['(
  "(require/untyped-contract"
  "  [check-array-shape"
  "   (-> (Vectorof Integer) (Vectorof Natural))])")]

This form comes with a surprising design choice.
If an untyped-contract identifier flows back into typed code,
 the type checker uses the original type rather than the supertype.
For @|sdeep| code, the choice is convenient because more programs can type-check
 using the supertype.
For @|sshallow|, though, the convenience disappears.
A @|sshallow| client must receive the wrapped version of the identifier,
 which means @|sshallow| code must behave in accordance with the supertype;
 hence, the @|sshallow| type checker uses the supertype as well.
Consequently, some well-typed @|sdeep| programs raise type errors upon switching
 to @|sshallow| types.

@; define-typed/untyped-identifier
@; TODO more extreme version of untyped-contract

The second problematic form is @tt{define-typed/untyped-identifier},
 which creates a new identifier from two old ones.
The following example defines @tt{f} from two other names:

@exact{\smallskip}
@typed-codeblock['(
  "(define-typed/untyped-identifier f"
  "  typed-f"
  "  untyped-f)")]

@|noindent|The meaning of the new @tt{f} depends on the context in which it appears.
In typed code, @tt{f} expands to @tt{typed-f}.
In untyped code, an @tt{f} is a synonym for @tt{untyped-f}.

The @tt{typed-f} is intended for @|sdeep|-typed code.
It cannot be safely used in a @|sshallow| module because it may
 assume type invariants.
Consequently, @|sshallow| code gets the untyped id.
This means, unfortunately, that changing a @|sdeep| module to @|sshallow|
 can raise a type checking error because occurrences of @tt{f} that expand to
 @tt{untyped-f} are plain, untyped identifiers.
There is no way to uncover the type that a @tt{typed-f} would have, and
 anyway there is no guarantee that @tt{typed-f} and @tt{untyped-f} have
 the same behavior.

For now, such type errors call for programmer-supplied annotations in
 the @|sshallow| client code.
In the future, this  @;@tt{define-typed/untyped-identifier}
 form would benefit
 from a third argument that specifies behavior in @|sshallow| contexts.


