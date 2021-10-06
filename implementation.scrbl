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

@title[#:tag "sec:implementation"]{Theory to Practice: Typed Racket}
@; 2021-04-29 do we really need to discuss typed/untyped API? could point to dissertation for details

According to the model, a mixed-typed language can support three-way
interactions by compiling types to contracts at @|sdeep|-typed boundaries
and to checks throughout @|sshallow|-typed code.
The model, however, is an abstraction that ignores practical issues.
To show that a realistic language can also support @|sdeep| and @|sshallow|
types, the authors have extended Typed Racket to combine its standard
@|snatural| semantics@~cite{tf-popl-2008} with a @|stransient| one@~cite{glfd-draft-2021}.
The two semantics coexist as modes of one compiler codebase and rely on the
same static type checker.
Unexpected challenges arose regarding the use of type definitions across
@|sdeep| and @|sshallow| modules and the adaptation of an API that customizes
@|sdeep|--@|suntyped| boundaries.


@section[#:tag "sec:implementation:overview"]{User-Facing API}

All Racket modules begin with a @tt{#lang} declaration.
The combined language introduces two concepts,
@|sDeep| Typed Racket and @|sShallow| Typed Racket,
and thus offers three basic module languages:
@itemlist[
@item{
  @tt{#lang typed/racket/deep} provides the @|sdeep| @|snatural| semantics, which
  enforces the types at module boundaries with higher-order contracts
  (same as the @tt{typed/racket} language).
}
@item{
  @tt{#lang typed/racket/shallow} provides a @|sshallow|, @|stransient| semantics that
  checks type shapes at all boundaries and all elimination forms.
}
@item{
  @tt{#lang racket} provides the untyped Racket semantics.
}
]

Both @|sdeep| and @|sshallow| modules rely on the same type checker, thus
programmers can easily switch between them.
If a module is well-typed in the @|sdeep| language, then with few exceptions
(@section-ref{sec:implementation:impl:tu}) it is well-typed in @|sshallow|.
Same goes for the reverse direction, when changing the @tt{#lang} line
from @|sshallow| to @|sdeep|.

@|sShallow| types are more permissive than @|sdeep| types at run-time.
Operationally, first-order checks enforce fewer properties than higher-order
contracts.
Thus, the set of programs that run to completion with @|sshallow| types is
a superset of the programs that run with @|sdeep| types.
@Section-ref{sec:evaluation:expressiveness} explores this semantic gap in greater detail.


@section[#:tag "sec:implementation:internals"]{Three-way Internals}

The Racket compiler processes an untyped module in two steps.
First, the macro expander simplifies expressions within the module to
a kernel language@~cite{f-icfp-2002,f-popl-2016}.
Second, the compiler backend optimizes the kernel code and tranforms it to a
native representation@~cite{fddkmstz-icfp-2018}.

@|sDeep| Racket injects three passes between the macro expander
and the compiler backend@~cite{tscff-pldi-2011}.
The first pass is the type checker.
The second pass generates contracts for module exports.
The final pass is a type-directed optimizer@~cite{stff-padl-2012} that generates
efficient untyped code for the compiler backend to further optimize.

@|sShallow| Racket is implemented as a second mode of the Typed Racket compiler@~cite{g-thesis-2020,glfd-draft-2021}.
It too injects three passes between the macro expander and compiler backend,
but with changes appropratie to the @|stransient| semantics.
The first pass, the type checker, remains unchanged.
The second pass is entirely different.
Instead of generating contracts at boundaries, @|sShallow| Racket generates
first-order checks and additionally traverses the module to put a check around
every potentially-unsafe elimination form.
The final pass is an optimizer that performs a subset of the @|sDeep| Racket optimizations.
Every optimization that is sound for weak type soundness is in the subset.
Others, notably the dead-code elimination pass, stay out.

The unexpected challenges for the three-way implementation arose in two
places: at the boundaries between @|sDeep| and @|sShallow| modules
and in Typed Racket's boundary API.


@subsection[#:tag "sec:implementation:impl:code"]{@|sDeep| and @|sShallow| Interaction}

Racket supports both separate compilation and hygienic macros@~cite{f-icfp-2002}.
Each module in a program compiles to a core language and other modules can
re-use the output.
Typed Racket cooperates with the separate compilation protocol by serializing
 the results of type checking@~cite{tscff-pldi-2011,ctf-sfp-2007}.
A well-typed module compiles to untyped code (with appropriate contracts)
 and a local type environment.
When one @|sDeep| module imports from another, the complied environment enables
faster type checking.

At first glance, it appears that @|sShallow| code can use the same protocol
 to find the type of @|sDeep| imports.
Wrappers get in the way, unfortunately, because of how @|sDeep| Racket protects
its exports.
For every public identifier in a @|sDeep| module, the compiler generates
a public macro that can expand to either the typed identifier or an untyped wrapper.
@|sShallow| code must use the wrappers, thus it also needs a way to uncover
their static types.
A similar problem arises when @|sDeep| code imports an identifier from
@|sShallow| Racket.

For @|sdeep|-to-@|sshallow| exports, the solution is to modify
type lookups to pass through wrappers.
Fortunately, the change is possible through the compile-time API for
Racket contracts.
The compiler associates each wrapped identifier with a structure that links
back to the original identifier.
The @|sShallow| type checker looks out for these wrappers and uncovers
the originals as needed.

@|sShallow|-to-@|sDeep| exports use a dual method.
The type checker inspects wrapped values to see whether there is an
available type.
Such types allow static type checks to succeed, and at run-time the wrapper
keeps @|sDeep| code safe.

Consequently, a @|sShallow| module must
be prepared to create wrappers for its exports.
The wrapper-making code is generated during compilation, at the end of
type checking, but it does not run until needed by a @|sDeep| client.
In this way, only programs that depend on @|sDeep| code suffer from the
expressiveness limits of wrappers.


@subsection[#:tag "sec:implementation:impl:syntax"]{Syntax Re-Use}

One limitation is that @|sShallow| code cannot use macros from @|sDeep| Racket modules.
Re-use is desirable to avoid copying code, but it requires a static analysis
 to enforce soundness.
Consider the following simple macro.
It applies a typed function @tt{f} to an input, and is consequently
 unsafe to send from @|sDeep| to @|sShallow| code:

@typed-codeblock['("(define-syntax-rule (call-f x) (f x))")]

@|noindent|If this macro could appear in a @|sShallow| Racket context, then any
 @|sShallow| value @tt{x} could sneak into the @|sDeep| function.
Unless @tt{f} makes no assumptions about its input, such values can break
 the @|sDeep| soundness guarantee and lead to dangerous results in optimized
 code.

One possible fix is to put a contract around every @|sDeep| identifier that
 appears in a macro.
Doing so would require an analysis to find out which contracts are needed,
 and a second analysis to install contracts wisely;
 each identifier requires a contract, but repeated occurrences of one identifier
 should not lead to repeated contract checks.
It may also be possible to avoid the contracts if the macro goes only to @|sDeep| clients.

Another possibility is to statically check whether a macro is safe to
 export.
Safe macros appear, for example, in the typed compatibility layer for the
 RackUnit testing library.
RackUnit is an untyped library that exports some functions and some macros.
The typed layer provides types for the functions and type-annotated copies
 of the macros (about 300 lines in total).
For example, the following macro combines a sequence of expressions into
 a named RackUnit test case:

@typed-codeblock['(
  "(define-syntax (test-case stx)"
  " (syntax-parse stx"
  "  [(_ name expr ...)"
  "   #'(parameterize ([test-name (ensure-str name)])"
  "       (test-begin expr ...))]))"
)]

@|noindent|This macro is safe for @|sShallow| clients, but for complicated reasons.
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

Currently, the author of a @|sDeep| library can enable syntax re-use by
disabling the optimizer and unsafely providing macros.
This work-around requires a manual inspection, but it is more appealing than
forking the RackUnit library and asking programmers to import the version
that matches their use own of @|sDeep| and @|sShallow|.


@subsection[#:tag "sec:implementation:impl:tu"]{@|sDeep|--@|sUntyped| Utilities}
@; struggles = edit old code , 2 of these => new type errors

Typed Racket has a small API by Neil Toronto to let programmers control boundaries
 between @|sdeep| and @|suntyped| code.
The API arose over time, as programmers (including Neil) discovered challenges.
Two forms in this API can lead to type errors due to the existence of
 @|sShallow| Racket code.

@; require/untyped-contract

The first problem concerns @tt{require/untyped-contract}.
This form lets untyped code import a typed identifier whose precise type
 cannot be expressed with a @|sdeep| contract.
Users supply a supertype of the precise type and @|sDeep| Racket uses this
 weaker type to generate a contract.
For example, the @bm{jpeg} benchmark (@section-ref{sec:evaluation:performance})
depends on a library for multi-dimensional
arrays (@render-lib[(make-lib "math/array" "https://docs.racket-lang.org/math/array.html")]).
This library accepts two kinds of data for array indices:
 either a vector of natural numbers or a vector of integers.
Helper functions check that values with the integer type do not actually
 contain negative numbers at run-time:

@exact{\smallskip}
@typed-codeblock['(
  "(: check-array-shape"
  "   (-> (U (Vectorof Natural) (Vectorof Integer))"
  "       (Vectorof Natural)))")]

@|noindent|Racket contracts cannot express the @|sDeep| Racket type of the checking
 function because they lack support for higher-order unions.
The work around is to impose a supertype on untyped clients:

@exact{\smallskip}
@untyped-codeblock['(
  "(require/untyped-contract"
  "  [check-array-shape"
  "   (-> (Vectorof Integer) (Vectorof Natural))])")]

The @tt{require/untyped-contract} form comes with an odd design choice.
If an untyped-contract identifier flows back into typed code,
the type checker uses the original type and the unwrapped value.
For @|sDeep| code, this choice is convenient.
For @|sShallow| code, the convenience is unsound.
A @|sShallow| client must instead receive the wrapped version of the identifier,
which means that the client must behave in accordance with the supertype.
Consequently, some well-typed @|sDeep| programs can raise type errors
after a one-line switch to @|sShallow| Racket.

@; define-typed/untyped-identifier
@; TODO more extreme version of untyped-contract

The second problematic form is @tt{define-typed/untyped-identifier},
 which creates a new identifier from two old ones.
The following example defines @tt{f} from two other names:

@exact{\smallskip}
@typed-codeblock['(
  "(define-typed/untyped-identifier f typed-f untyped-f)")]

@|noindent|The meaning of @tt{f} depends on the context in which it appears.
In typed code, @tt{f} expands to @tt{typed-f}.
In untyped code, an @tt{f} is a synonym for @tt{untyped-f}.

The @tt{typed-f} is intended for @|sDeep| Racket code;
it cannot be safely used in a @|sShallow| module because it may
 assume full type soundness.
Consequently, @|sShallow| code gets the untyped id.
This means, unfortunately, that changing a @|sDeep| module to @|sShallow|
 can raise a type checking error because occurrences of @tt{f} that expand to
 @tt{untyped-f} are plain, untyped identifiers.
There is no easy way to uncover the type that a @tt{typed-f} would have,
but more importantly there is no guarantee that @tt{typed-f} and @tt{untyped-f}
have the same behavior.
In the future, the @tt{define-typed/untyped-identifier}
 form would benefit
 from a third argument that specifies behavior in @|sShallow| contexts.


