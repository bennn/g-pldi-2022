#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in scriblib/footnote note)
   (only-in "pict.rkt" fig:model-interaction-y))

@; THESIS Deep and Shallow can interoperate
@; - don't worry about CM extension, for self-edges and S/U
@;   - (2021-04-25) that means, don't present the self-loops? or don't focus on them?
@; - don't worry about false-start designs ... point to dissertation for now

@title[#:tag "sec:model"]{Model and Metatheory}

A typical mixed-typed language allows for two styles of code, typed and untyped,
and uses run-time checks to enforce the claims made by static types.
The model here allows three styles:
@|sdeep|-typed code, @|sshallow|-typed code, and @|suntyped| code.
Both the @|sdeep| and @|sshallow| code must satisfy a type checker that
validates typical well-formedness properties.
The @|suntyped| code has fewer constraints, but is nevertheless free to
communicate with @|sdeep| and @|sshallow| chunks of code.

Overall, the primary goal of the model is to test whether @|sdeep|, @|sshallow|,
and @|suntyped| code can safely interoperate.
The @|sdeep| types must satisfy type soundness and complete monitoring properties,
and the @|sshallow| types must satisfy only a weak type soundness.
A secondary goal of the model is to serve as the outline for an implementation.
For this reason, the three syntaxes compile to one kernel language
that employs standard run-time-checking concepts.
A @emph[swrap] term corresponds to a higher-order contract,
a @emph[sscan] performs a first-order check,
and a @emph[snoop] does nothing.
@Figure-ref{fig:model:base-interaction} sketches the plan for applying these checks
at boundaries.
@; @Sectionref{sec:model:model:theorems} proves the main result;
@; namely, that a careful use of these checks can enforce the safety properties.

@figure[
  "fig:model:base-interaction"
  @elem{Outline for @|sdeep|, @|sshallow|, and @|suntyped| interactions}
  fig:model-interaction-y]


@section[#:tag "sec:model:model:syntax"]{Three-way Surface Syntax}

@figure[
  "fig:model:surface"
  @elem{Surface syntax}

  @exact|{
\begin{langarray}
  \ssurface & \slangeq &
    \svar \mid \sint \mid \epair{\ssurface}{\ssurface} \mid
    \efun{\svar}{\ssurface}
    \mid \efun{\tann{\svar}{\stype}}{\ssurface}
    \mid \efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface} \mid
  \\ & &
    \eunop{\ssurface} \mid \ebinop{\ssurface}{\ssurface} \mid
    \eappu{\ssurface}{\ssurface} \mid
    \emod{\slang}{\ssurface}
  \\
  \slang & \slangeq &
    \sD \mid \sS \mid \sU
  \\
  \stype & \slangeq &
    \tnat \mid \tint \mid \tpair{\stype}{\stype} \mid \tfun{\stype}{\stype}
  \\
  \stspec & \slangeq &
    \stype \mid \tfloor{\stype} \mid \tdyn
\end{langarray}
}|]

The surface syntax (@figure-ref{fig:model:surface}) equips a basic expression
language with optional type annotations and module boundaries.
Surface expressions @${\ssurface} consist of function applications (@${\eappu{\ssurface}{\ssurface}}),
 primitive operation applications (@${\eunop{\ssurface}}, @${\ebinop{\ssurface}{\ssurface}}),
 variables @${\svar},
 integers @${\sint},
 pairs @${\epair{\ssurface}{\ssurface}},
 and optionally-annotated functions.
An @|suntyped| function has no annotation (@${\efun{\svar}{\ssurface}}),
 a @|sdeep|-typed function has a plain type annotation (@${\efun{\tann{\svar}{\stype}}{\ssurface}}),
 and a @|sshallow|-typed function has an underlined type annotation (@${\efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface}}).
The underline is simply a notational device; it is meant to suggest that only
the top-level shape of this type is guaranteed at run-time.
Types @${\stype} express natural numbers (@${\tnat}),
 integers (@${\tint}),
 pairs (@${\tpair{\stype}{\stype}}),
 and functions (@${\tfun{\stype}{\stype}}).
Modules associate a label with an expression (@${\emod{\slang}{\ssurface}}).
The label @${\slang} is either @${\sD} for @|sdeep|-typed code,
 @${\sS} for @|sshallow|-typed code, or @${\sU} for @|suntyped| code.
For example, the term @${(\emod{\sD}{\ssurface_0})} says that @${\ssurface_0}
is a @|sdeep|-typed expression.
Any module expressions within @${\ssurface_0} are free to use either the same
typing style or a different one.


@section[#:tag "sec:model:model:types"]{Three-way Surface Typing}
@; TODO why not reformat, present as 2 or 3 judgments?
@;  the evaluation typing is 3 judgments (different invariants)

Both @|sdeep| and @|sshallow| code must satisfy type constraints,
and @|suntyped| code cannot reference variables that it did not bind.
These well-formedness conditions are spelled out in the typing judgment
of @figure-ref{fig:model:surface-type}, which relates a type environment
@${\stypeenv} and an expression @${\ssurface} to a result specification.
A result @${\stspec} is either a type @${\stype} for @|sdeep|-typed code,
an underlined type @${\tfloor{\stype}} for @|sshallow|-typed code,
or the uni-type @${\tdyn} for @|suntyped| code.

With the exception of modules, the typing rules are standard for a basic
functional language.
Note that the subsumption rule means the judgment is not syntax-directed.
The rules for modules allow one kind of expression to appear within another.
For instance, an @|suntyped| expression may appear within a @|sdeep|
expression @${\ssurface_0} via a module boundary;
if so, the full expression @${\ssurface_0} satisfies the typing judgment if the
@|sdeep| parts are well-typed and the @|suntyped| parts are well-formed.

@Figureref{fig:model:extra-type} defines the subtyping judgment (@${\ssubt})
and a type-assignment for primitive operations (@${\sDelta}).
These are both standard.
Subtyping declares that the natural numbers are a subset of the integers
and extends this decree covariantly to pairs and contra/co-variantly to
function domains/codomains.
The primitive operations are overloaded to combine natural numbers or
integers.

@;One way to comprehend the typing judgment is as three separate judgments
@;for @|sdeep|, @|sshallow|, and @|suntyped| code.
@;Grouping the rules by their result specification and focusing on each group
@;in sequence may make for easier reading.
@;Ultimately, though, module expressions call for mutual dependencies.
@;
@;- if separate judgments, would need common type environment
@;- PS deep and shallow are identical but for underline, matters for implementation



@figure[
  "fig:model:surface-type"
  @elem{Surface typing (selected rules), subtyping, and types for primitive operations}
  @;@appendixref{appendix:rules}

@exact|{
\lbl{\fbox{\(\stypeenv \sST \ssurface : \stspec\)}}{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\tdyn} \in \stypeenv
  }{
    \stypeenv \sST \svar_0 : \tdyn
  }

  \inferrule*{
    \tann{\svar_0}{\stype_0} \in \stypeenv
  }{
    \stypeenv \sST \svar_0 : \stype_0
  }

  \inferrule*{
    \tann{\svar_0}{\tfloor{\stype_0}} \in \stypeenv
  }{
    \stypeenv \sST \svar_0 : \tfloor{\stype_0}
  }

  \inferrule*{
  }{
    \stypeenv \sST \snat_0 : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv \sST \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv \sST \snat_0 : \tfloor{\tnat}
  }

%  \inferrule*{
%  }{
%    \stypeenv \sST \sint_0 : \tint
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sST \sint_0 : \tfloor{\tint}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \tdyn
%    \\
%    \stypeenv \sST \ssurface_1 : \tdyn
%  }{
%    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \stype_0
%    \\
%    \stypeenv \sST \ssurface_1 : \stype_1
%  }{
%    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tpair{\stype_0}{\stype_1}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \tfloor{\stype_0}
%    \\
%    \stypeenv \sST \ssurface_1 : \tfloor{\stype_1}
%  }{
%    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tfloor{\tpair{\stype_0}{\stype_1}}
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sST \sexpr_0 : \tdyn
  }{
    \stypeenv \sST \efun{\svar_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv} \sST \sexpr_0 : \stype_1
  }{
    \stypeenv \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tfloor{\stype_0}}}{\stypeenv} \sST \sexpr_0 : \tfloor{\stype_1}
  }{
    \stypeenv \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}}
  }

%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \tdyn
%  }{
%    \stypeenv \sST \eunop{\ssurface_0} : \tdyn
%  }
%
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\\\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \stype_1
  }

%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
%    \\
%    \sDelta(\sunop, \stype_0) = \stype_1
%  }{
%    \stypeenv \sST \eunop{\sexpr_0} : \tfloor{\stype_1}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \tdyn
%    \\
%    \stypeenv \sST \ssurface_1 : \tdyn
%  }{
%    \stypeenv \sST \ebinop{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0
%    \\
%    \stypeenv \sST \sexpr_1 : \stype_1
%    \\\\
%    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
%  }{
%    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
%    \\
%    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1}
%    \\\\
%    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
%  }{
%    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \tfloor{\stype_2}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \ssurface_0 : \tdyn
%    \\
%    \stypeenv \sST \ssurface_1 : \tdyn
%  }{
%    \stypeenv \sST \eappu{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfun{\stype_0}{\stype_1}
%    \\
%    \stypeenv \sST \sexpr_1 : \stype_0
%  }{
%    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \stype_1
%  }
%
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_0}{\stype_1}}
    \\\\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \tfloor{\stype_1}
  }

%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0
%    \\
%    \fsubt{\stype_0}{\stype_1}
%  }{
%    \stypeenv \sST \sexpr_0 : \stype_1
%  }
%
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
    \\\\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_1}
  }

%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0
%  }{
%    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
%  }{
%    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \stype_0
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0
%  }{
%    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \stype_0
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
%  }{
%    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \stype_0
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \tfloor{\stype_0}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0
%  }{
%    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \tfloor{\stype_0}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
%  }{
%    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \tfloor{\stype_0}
%  }
\end{mathpar}

\medskip
\begin{tabular}{l@{\qquad}l}
\(\left[
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stspec_0
  }{
    \stypeenv \sST \emodule{\slang_0}{\sexpr_0} : \stspec_1
  }
\right]\) &
\(\begin{array}{llll}
  \slang_0 & \stspec_0 & \stspec_1 \\\hline
  \sU & \tdyn & \stspec_1 \\
  \sD & \stype_0 & \stspec_1 \\
  \sS & \tfloor{\stype_0} & \stspec_1 \\
\end{array}\) \end{tabular}
}

\medskip
\lbl{\fbox{$\fsubt{\stype}{\stype}$}}{\vspace{-0.6cm}\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\tnat}{\tint}
  }

  \inferrule*{
    \fsubt{\stype_0}{\stype_2}
    \\\\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tpair{\stype_0}{\stype_1}}{\tpair{\stype_2}{\stype_3}}
  }

  \inferrule*{
    \fsubt{\stype_2}{\stype_0}
    \\\\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tfun{\stype_0}{\stype_1}}{\tfun{\stype_2}{\stype_3}}
  }
\end{mathpar}}

\medskip
\begin{minipage}[t]{0.48\columnwidth}
\lbl{\fbox{$\sDelta : \ffun{\tpair{\sunop\,}{\,\stype}}{\stype}$}}{
  \begin{langarray}
    \sDelta(\sfst, \tpair{\stype_0}{\stype_1}) & \feq & \stype_0
  \\
    \sDelta(\ssnd, \tpair{\stype_0}{\stype_1}) & \feq & \stype_1
  \end{langarray}
}
\end{minipage}\begin{minipage}[t]{0.52\columnwidth}
\lbl{\fbox{$\sDelta : \ffun{\tpair{\sbinop\,}{\tpair{\,\stype\,}{\,\stype}}}{\stype}$}}{
  \begin{langarray}
    \sDelta(\ssum, \tnat, \tnat) & \feq & \tnat
  \\
    \sDelta(\ssum, \tint, \tint) & \feq & \tint
  \\
    \sDelta(\squotient, \tnat, \tnat) & \feq & \tnat
  \\
    \sDelta(\squotient, \tint, \tint) & \feq & \tint
  \end{langarray}
}
\end{minipage}

}|]

@;@figure[
@;  "fig:model:extra-type"
@;  @elem{Types for primitive operations.}
@;
@;  @exact|{
@;%\lbl{\fbox{$\fsubt{\stype}{\stype}$}}{\begin{mathpar}
@;%  \inferrule*{
@;%  }{
@;%    \fsubt{\tnat}{\tint}
@;%  }
@;%
@;%  \inferrule*{
@;%    \fsubt{\stype_0}{\stype_2}
@;%    \\
@;%    \fsubt{\stype_1}{\stype_3}
@;%  }{
@;%    \fsubt{\tpair{\stype_0}{\stype_1}}{\tpair{\stype_2}{\stype_3}}
@;%  }
@;%
@;%  \inferrule*{
@;%    \fsubt{\stype_2}{\stype_0}
@;%    \\
@;%    \fsubt{\stype_1}{\stype_3}
@;%  }{
@;%    \fsubt{\tfun{\stype_0}{\stype_1}}{\tfun{\stype_2}{\stype_3}}
@;%  }
@;%\end{mathpar}}
@;
@;\begin{minipage}[t]{0.48\columnwidth}
@;\lbl{\fbox{$\sDelta : \ffun{\tpair{\sunop\,}{\,\stype}}{\stype}$}}{
@;  \begin{langarray}
@;    \sDelta(\sfst, \tpair{\stype_0}{\stype_1}) & \feq & \stype_0
@;  \\
@;    \sDelta(\ssnd, \tpair{\stype_0}{\stype_1}) & \feq & \stype_1
@;  \end{langarray}
@;}
@;\end{minipage}\begin{minipage}[t]{0.52\columnwidth}
@;\lbl{\fbox{$\sDelta : \ffun{\tpair{\sbinop\,}{\tpair{\,\stype\,}{\,\stype}}}{\stype}$}}{
@;  \begin{langarray}
@;    \sDelta(\ssum, \tnat, \tnat) & \feq & \tnat
@;  \\
@;    \sDelta(\ssum, \tint, \tint) & \feq & \tint
@;  \\
@;    \sDelta(\squotient, \tnat, \tnat) & \feq & \tnat
@;  \\
@;    \sDelta(\squotient, \tint, \tint) & \feq & \tint
@;  \end{langarray}
@;}
@;\end{minipage}
@;}|]


@section[#:tag "sec:model:model:eval-syntax"]{Common Evaluation Syntax}

By contrast to the declarative surface syntax, the purpose of the evaluation
syntax is to present a core set of expressions that can support three-way
interactions.
The syntax removes surface terms that merely express an intent and adds
terms that directly map to run-time checks.

Evaluation expressions @${\sexpr} consist of variables, values, primitive
applications, function applications, errors, and boundary terms.
The module boundaries from the surface syntax are gone.
Instead, three @emph{boundary terms} describe run-time checks.
A @|swrap| boundary asks for the full enforcement of a type, either with a comprehensive first-order
check or a higher-order wrapper;
a @|sscan| boundary asks for a first-order type-shape (@${\sshape}) check;
and a @|snoop| boundary asks for no check.

Together, values and errors represent the possible results of an evaluation.
A value is either an integer, a pair, a function, or a guard wrapper.
A guard @${(\emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0})} is a wrapped
function; more precisely, it provides access to the function @${\svalue_0}
subject to type-directed checks.
@|sShallow|-typed functions have a shape annotation and a @${\sscan} tag in
the evaluation syntax (@${\esfun{\svar}{\sshape}{\sexpr}})
to suggest that such functions must validate the shape of their input at run-time.
An error may arise from either a failed check at @|swrap| boundary (@${\swraperror}),
a failed check at a @|sscan| boundary (@${\sscanerror}), a division by zero
(@${\sdivzeroerror}), or a malformed untyped expression (@${\stagerror}).

@; other notes 2021-06-08
@; - shape-ann on functions reflects runtime knowledge ... and also the dom-check to insert
@; - example tag error = app 3 4


@figure[
  "fig:model:eval-syntax"
  @elem{Evaluation Syntax}

  @exact|{
\begin{langarray}
  \sexpr & \slangeq &
    \svar \mid \svalue \mid \epair{\sexpr}{\sexpr}
    \mid \eunop{\sexpr} \mid
    \ebinop{\sexpr}{\sexpr} \mid \eappu{\sexpr}{\sexpr} \mid
  \\ & &
    \serror
    \mid \ewrap{\stype}{\sexpr}
    \mid \escan{\sshape}{\sexpr}
    \mid \enoop{\sexpr}
  \\
  \svalue & \slangeq &
    \sint \mid \epair{\svalue}{\svalue}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \esfun{\svar}{\sshape}{\sexpr} \mid
  \\ & &
    \emon{(\tfun{\stype}{\stype})}{\svalue}
  \\
  \sshape & \slangeq &
    \knat \mid \kint \mid \kpair \mid \kfun \mid \kany
  \\
  \serror & \slangeq &
    \swraperror \mid \sscanerror \mid \sdivzeroerror \mid \stagerror
  \\
  \sctx & \slangeq &
    \sctxhole \mid \eunop{\sctx} \mid \ebinop{\sctx}{\sexpr} \mid \ebinop{\svalue}{\sctx}
    \mid \eappu{\sctx}{\sexpr} \mid
  \\ & &
    \eappu{\svalue}{\sctx} \mid \enoop{\sctx} \mid \escan{\sshape}{\sctx} \mid \ewrap{\stype}{\sctx}
\end{langarray}
}|]


@section[#:tag "sec:model:model:eval-types"]{Three-way Evaluation Typing}

The evaluation syntax comes with three typing judgments that describe the
run-time invariants of @|sdeep|, @|sshallow|, and @|suntyped| code.
The @|sdeep| typing judgment (@${\sWTD}) validates full types.
The @|sshallow| judgment (@${\sWTS}) checks top-level type shapes.
And the @|suntyped| judgment (@${\sWTU}) checks that all variables
have proper bindings.

Both the @|sdeep| and @|suntyped| rules are similar to the corresponding
surface-language rules because they support equally-strong conclusions
(full types and the unitype).
The @|sshallow| judgment is rather different because it validates type
shapes instead of full (underlined @${\tfloor{\stype}}) types.
When inspecting a pair, for example, the judgment concludes with the @${\kpair}
shape no matter what shapes the elements have.
Consequently, a pair elimination form such as @${(\efstu{\svar_0})} has the
@${\kany} shape because the pair may contain any sort of value.
Similar comments apply to functions and applications.
Thus, if a program expects a particular shape from the element of a pair or the
range of a function, then the program must use a @${\sscan} assertion to
confirm this expectation.

@figure[
  "fig:model:deep-type"
  @elem{@|sDeep| typing judgment (selected rules)}
  @;@appendixref{appendix:rules}

@exact|{
\lbl{\fbox{\(\stypeenv \sWTT \sexpr : \stype\)}}{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\stype_0} \in \stypeenv
  }{
    \stypeenv \sWTT \svar_0 : \stype_0
  }

%  \inferrule*{
%  }{
%    \stypeenv \sWTT \snat_0 : \tnat
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sWTT \sint_0 : \tint
%  }
%
%  \inferrule*{
%    \stypeenv \sWTT \sexpr_0 : \stype_0
%    \\
%    \stypeenv \sWTT \sexpr_1 : \stype_1
%  }{
%    \stypeenv \sWTT \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1}
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv} \sWTT \sexpr_0 : \stype_1
  }{
    \stypeenv \sWTT \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}
  }

  \inferrule*{
    \stypeenv \sWTU \svalue_0 : \tdyn
  }{
    \stypeenv \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sWTS \svalue_0 : \sshape_0
  }{
    \stypeenv \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

%  \inferrule*{
%    \stypeenv \sWTT \sexpr_0 : \stype_0
%    \\\\
%    \sDelta(\sunop, \stype_0) = \stype_1
%  }{
%    \stypeenv \sWTT \eunop{\sexpr_0} : \stype_1
%  }
%
%  \inferrule*{
%    \stypeenv \sWTT \sexpr_0 : \stype_0
%    \\
%    \stypeenv \sWTT \sexpr_1 : \stype_1
%    \\\\
%    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
%  }{
%    \stypeenv \sWTT \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
%  }
%
  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \tfun{\stype_0}{\stype_1}
    \\
    \stypeenv \sWTT \sexpr_1 : \stype_0
  }{
    \stypeenv \sWTT \eappu{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv \sWTT \enoop{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTT \ewrap{\stype_0}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv \sWTT \ewrap{\stype_0}{\sexpr_0} : \stype_0
  }

%  \inferrule*{
%    \stypeenv \sWTT \sexpr_0 : \stype_0
%    \\
%    \fsubt{\stype_0}{\stype_1}
%  }{
%    \stypeenv \sWTT \sexpr_0 : \stype_1
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sWTT \serror : \stype_0
%  }
\end{mathpar}
}}|]

@figure[
  "fig:model:shallow-type"
  @elem{@|sShallow| typing (selected rules), subtyping, and type-to-shape metafunction}
  @;@appendixref{appendix:rules}

@exact|{
\lbl{\fbox{\(\stypeenv \sWTS \sexpr : \sshape\)}}{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sshape_0} \in \stypeenv
  }{
    \stypeenv \sWTS \svar_0 : \sshape_0
  }

%  \inferrule*{
%  }{
%    \stypeenv \sWTS \snat_0 : \tnat
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sWTS \sint_0 : \tint
%  }
%
%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_0
%    \\
%    \stypeenv \sWTS \sexpr_1 : \sshape_1
%  }{
%    \stypeenv \sWTS \epair{\sexpr_0}{\sexpr_1} : \kpair
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTS \efun{\svar_0}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTS \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \stypeenv \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv \sWTS \emon{\stype_0}{\svalue_0} : \kfun
  }

%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_0
%  }{
%    \stypeenv \sWTS \eunop{\sexpr_0} : \kany
%  }
%
%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_0
%    \\
%    \stypeenv \sWTS \sexpr_1 : \sshape_1
%    \\
%    \sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2
%  }{
%    \stypeenv \sWTS \ebinop{\sexpr_0}{\sexpr_1} : \sshape_2
%  }
%
  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \kfun
    \\
    \stypeenv \sWTS \sexpr_1 : \sshape_0
  }{
    \stypeenv \sWTS \eappu{\sexpr_0}{\sexpr_1} : \kany
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv \sWTS \enoop{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    %% why need this???
    %% 2021-04-25 : because no way to check 'any' ... it should be a scan ... let's fix that
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTS \enoop{\sexpr_0} : \kany
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTS \escan{\sshape_0}{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTS \escan{\sshape_0}{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv \sWTS \ewrap{\stype_0}{\sexpr_0} : \sshape_0
  }

%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_0
%    \\
%    \fsubt{\sshape_0}{\sshape_1}
%  }{
%    \stypeenv \sWTS \sexpr_0 : \sshape_1
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sWTS \serror : \sshape_0
%  }
\end{mathpar}
}

\medskip
\lbl{\fbox{$\fsubt{\sshape}{\sshape}$}}{\vspace{-0.6cm}\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\knat}{\kint}
  }

  \inferrule*{
  }{
    \fsubt{\sshape_0}{\kany}
  }
\end{mathpar}}

\medskip
\lbl{\fbox{$\sshapecheck : \ffun{\stype}{\sshape}$}}{
\begin{tabular}{ll}
  \begin{langarray}
    \fshape{\tnat} & \feq & \knat
  \\
    \fshape{\tint} & \feq & \kint
  \end{langarray} &
  \begin{langarray}
    \fshape{\tpair{\stype_0}{\stype_1}} & \feq & \kpair
  \\
    \fshape{\tfun{\stype_0}{\stype_1}} & \feq & \kfun
  \end{langarray}
\end{tabular}}
}|]


@figure[
  "fig:model:untyped-type"
  @elem{Untyped typing judgment (selected rules)}
  @;@appendixref{appendix:rules}
  @; ... aka dynamic typing, most types checked at runtime

@exact|{
\lbl{\fbox{\(\stypeenv \sWTU \sexpr : \tdyn\)}}{\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\tdyn} \in \stypeenv
  }{
    \stypeenv \sWTU \svar_0 : \tdyn
  }

%  \inferrule*{
%  }{
%    \stypeenv \sWTU \sint_0 : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sWTU \sexpr_0 : \tdyn
%    \\
%    \stypeenv \sWTU \sexpr_1 : \tdyn
%  }{
%    \stypeenv \sWTU \epair{\sexpr_0}{\sexpr_1} : \tdyn
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTU \efun{\svar_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTU \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv \sWTU \emon{\stype_0}{\svalue_0} : \tdyn
  }

%  \inferrule*{
%    \stypeenv \sWTU \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sWTU \eunop{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sWTU \sexpr_0 : \tdyn
%    \\
%    \stypeenv \sWTU \sexpr_1 : \tdyn
%  }{
%    \stypeenv \sWTU \ebinop{\sexpr_0}{\sexpr_1} : \tdyn
%  }
%
  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv \sWTU \eappu{\sexpr_0}{\sexpr_1} : \tdyn
  }

%  \inferrule*{
%    \stypeenv \sWTU \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sWTU \enoop{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_0
%  }{
%    \stypeenv \sWTU \enoop{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sWTU \sexpr_0 : \tdyn
%  }{
%    \stypeenv \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sWTS \sexpr_0 : \sshape_1
%  }{
%    \stypeenv \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
%  }
%
  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv \sWTU \ewrap{\stype_0}{\sexpr_0} : \tdyn
  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sWTU \serror : \tdyn
%  }
\end{mathpar}
}}|]

@;@figure[
@;  "fig:model:shallow-util"
@;  @elem{@|sShallow| subtyping and type-to-shape metafunction}
@;
@;@exact|{
@;\lbl{\fbox{$\fsubt{\sshape}{\sshape}$}}{\begin{mathpar}
@;  \inferrule*{
@;  }{
@;    \fsubt{\knat}{\kint}
@;  }
@;
@;  \inferrule*{
@;  }{
@;    \fsubt{\sshape_0}{\kany}
@;  }
@;\end{mathpar}}
@;
@;\lbl{\fbox{$\sshapecheck : \ffun{\stype}{\sshape}$}}{
@;  \begin{langarray}
@;    \fshape{\tnat} & \feq & \knat
@;  \\
@;    \fshape{\tint} & \feq & \kint
@;  \\
@;    \fshape{\tpair{\stype_0}{\stype_1}} & \feq & \kpair
@;  \\
@;    \fshape{\tfun{\stype_0}{\stype_1}} & \feq & \kfun
@;  \end{langarray}
@;}
@;}|]

@figure[
  "fig:model:completion1"
  @elem{Surface-to-evaluation completion (selected rules)}
  @;@appendixref{appendix:rules}

@exact|{
\lbl{\fbox{\(\stypeenv \sST \ssurface : \stspec \scompile \sexpr\)}}{\begin{mathpar}
  \inferrule*{
  }{
    \stypeenv \sST \svar_0 : \stspec_0 \scompile \svar_0
  }

%  \inferrule*{
%  }{
%    \stypeenv \sST \svar_0 : \stype_0 \scompile \svar_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sST \svar_0 : \tfloor{\stype_0} \scompile \svar_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sST \sint_0 : \tdyn \scompile \sint_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sST \sint_0 : \stype_0 \scompile \sint_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv \sST \sint_0 : \tfloor{\stype_0} \scompile \sint_0
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
%    \\\\
%    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
%  }{
%    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
%    \\\\
%    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
%  }{
%    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
%    \\\\
%    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
%  }{
%    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv \sST \efun{\svar_0}{\sexpr_0} : \tdyn \scompile \efun{\svar_0}{\sexpr_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv} \sST \sexpr_0 : \stype_1 \scompile \sexpr_1
  }{
    \stypeenv \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1} \scompile \efun{\tann{\svar_0}{\stype_0}}{\sexpr_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tfloor{\stype_0}}}{\stypeenv} \sST \sexpr_0 : \tfloor{\stype_1} \scompile \sexpr_1
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv \sST \efun{\tann{\svar_0}{\tfloor{\stype_0}}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}} \scompile \esfun{\svar_0}{\sshape_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \tdyn \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfun{\stype_1}{\stype_0} \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \stype_0 \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_1}{\stype_0}} \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{(\eappu{\sexpr_2}{\sexpr_3})}
  }
\end{mathpar}

\medskip
\begin{tabular}[t]{l@{~~}l}
\!\!\!\!\(\left[
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stspec_0 \scompile \sexpr_1
  }{
    \stypeenv\!\sST\!\emodule{\slang_0}{\sexpr_0}\!:\!\stspec_1\!\scompile\!\sexpr_2
  }
\right]\) &
\(\begin{array}{llll}
  \slang_0 & \stspec_0 & \stspec_1 & \scompile \sexpr_2 \!\! \\\hline
  \sD & \stype_0 & \tdyn & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sS & \tfloor{\stype_0} & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sU & \tdyn & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sD & \stype_0 & \tfloor{\stype_0} & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sS & \tfloor{\stype_0} & \tdyn & \enoop{\sexpr_1} \!\! \\
  \sU & \tdyn & \tfloor{\stype_0} & \escan{\sshape_0}{\sexpr_1} \!\!
\end{array}\)
\\ & \qquad\hbox{where \(\sshape_0 = \fshape{\stype_0}\)}

%  \sU & \tdyn & \tdyn & \enoop{\sexpr_1} \\
%  \sD & \stype_0 & \stype_0 & \enoop{\sexpr_1} \\
%  \sS & \tfloor{\stype_0} & \tfloor{\stype_0} & \enoop{\sexpr_1}
\end{tabular}

}}|]

@;@figure[
@;  "fig:model:completion2"
@;  @elem{Completion rules for module boundaries (@figure-ref{fig:model:base-interaction})}
@;
@;@exact|{
@;\left[
@;  \inferrule*{
@;    \stypeenv \sST \sexpr_0 : \stspec_0 \scompile \sexpr_1
@;  }{
@;    \stypeenv \sST \emodule{\slang_0}{\sexpr_0} : \stspec_1 \scompile \sexpr_2
@;  }
@;\right]
@;
@;\medskip
@;\begin{array}{llll}
@;  \slang_0 & \stspec_0 & \stspec_1 & \scompile \sexpr_2 \\\hline
@;  \sD & \stype_0 & \tdyn & \ewrap{\stype_0}{\sexpr_1} \\
@;  \sU & \tdyn & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \\
@;  \sS & \tfloor{\stype_0} & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \\
@;  \sD & \stype_0 & \tfloor{\stype_0} & \ewrap{\stype_0}{\sexpr_1} \\
@;  \sU & \tdyn & \tfloor{\stype_0} & \escan{\fshape{\stype_0}}{\sexpr_1} \\
@;  \sU & \tdyn & \tdyn & \enoop{\sexpr_1} \\
@;  \sS & \tfloor{\stype_0} & \tdyn & \enoop{\sexpr_1} \\
@;  \sD & \stype_0 & \stype_0 & \enoop{\sexpr_1} \\
@;  \sS & \tfloor{\stype_0} & \tfloor{\stype_0} & \enoop{\sexpr_1}
@;\end{array}
@;
@;}|]


@section[#:tag "sec:model:model:completion"]{Compilation from Surface to Evaluation}

The surface syntax has no semantics of its own.
Instead, a compilation pass maps surface terms to evaluation-language
terms with run-time checks.
Most checks appear at boundaries; indeed, the main task of compilation is
to replace surface module boundaries with check boundaries.
Additional checks appear in @|sshallow|-typed code to support its type
soundness property.

Because compilation inserts run-time checks and little more, it is more
like a completion pass that makes implicit operations explicit@~cite{h-scp-1994}
than a typical compiler.
Henceforth, this paper uses the term @emph{completion} instead of compilation.

The overall goal of completion-inserted checks is to map all well-typed surface
expressions to well-typed evaluation expressions (refer to the appendix
for a formal specification).
@itemlist[
@item{
 In @|sdeep|-typed code, completion inserts @|swrap| expressions at the
  module boundaries to less-typed code.
 Other @|sdeep| expressions have no checks.
}
@item{
 In @|sshallow| code, completion scans incoming untyped values at boundaries
 and scan the result of every elimination form.
}
@item{
 In untyped code, completion adds no run-time checks.
 At the boundaries to @|sdeep| and @|sshallow| code, however, the above
 strategy calls for a @|swrap| check.
}
]
@|noindent|The rules shown in @figureref["fig:model:completion2" "fig:model:completion1"]
say exactly how to insert the checks.
@Figure-ref{fig:model:completion2} presents the rules for module boundaries
using a tabular notation.
The parameterized judgment at the left of the figure summarizes the rules for
boundaries, and the tables at the right show the specific parameters for the
nine possible combinations.
These nine rules correspond to the six edges in
@figure-ref{fig:model:base-interaction} plus three @${\snoop} self-edges.

@Figureref{fig:model:completion1} illustrates the completion rules for
functions.
In @|sdeep|-typed and @|suntyped| code, the completion of an application is simply
the completion of its subexpressions.
In @|sshallow|-typed code, this elimination form requires a @|sscan| check to
validate the result.
Pairs and pair elimination forms follow a similar pattern.
The completion of a @|sshallow| function has a @${\sscan} tag in its domain
to indicate the need for a run-time check.
A static analysis might remove these tags if the @|sshallow| function never
escapes to an untyped context.
For other expressions, the completion rules simply transform subexpressions;
these rules appear in the appendix.


@figure*[
  "fig:model:rr"
  @; "fig:model:rrlbl"
  @elem{Semantics for the evaluation syntax (left) and labeled variant (right)}

@exact|{
\(\begin{array}{l@{\hspace{0.2em}}c@{\hspace{0.2em}}l@{\hspace{7mm}}l@{\hspace{0.2em}}c@{\hspace{0.2em}}l}
\!\!\!\!\fbox{\(\sexpr \snr \sexpr\)}
& & &
\!\!\!\!\fbox{\(\obars{\sexpr}{\sowner} \snrlbl \obars{\sexpr}{\sowner}\)} \\

\eunop{\svalue_0}
& \snr
& \stagerror

& \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\stagerror}{\sowner_1}
\\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is undefined}
& \sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sunop, \svalue_0)$ is undefined}
\\[1.0ex]


\eunop{\svalue_0}
& \snr
& \sdelta(\sunop, \svalue_0)

& \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}
\\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
& \sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
\\[1.0ex]


\ebinop{\svalue_0}{\svalue_1}
& \snr
& \stagerror

& \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
& \snrlbl
& \obars{\stagerror}{\sowner_2}
\\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
& \sidecond{if $\svalue_i \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
\\[1.0ex]


\ebinop{\svalue_0}{\svalue_1}
& \snr
& \sdelta(\sbinop, \svalue_0, \svalue_1)

& \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
& \snrlbl
& \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}
\\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
& \sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
\\[1.0ex]

\eappu{\svalue_0}{\svalue_1}
& \snr
& \stagerror

& \obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1}
& \snrlbl
& \obars{\stagerror}{\sowner_1}
\\\sidecond{if $\svalue_0 \not\in \efun{\svar}{\!\sexpr} \cup \efun{\tann{\svar}{\stype}}{\!\sexpr} \cup \esfun{\svar}{\sshape}{\!\sexpr} \cup \emon{\stype}{\!\svalue}$}
& \sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner} \cup \efun{\svar}{\!\sexpr} \cup \efun{\tann{\svar}{\stype}}{\!\sexpr} \cup \esfun{\svar}{\sshape}{\!\sexpr} \cup \emon{\stype}{\!\svalue}$}
\\[1.0ex]


\eappu{(\efun{\svar_0}{\sexpr_0})}{\svalue_0}
& \snr
& \esubst{\sexpr_0}{\svar_0}{\svalue_0}

& \obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
& \snrlbl
& \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
\\[1.0ex]


\eappu{(\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0})}{\svalue_0}
& \snr
& \esubst{\sexpr_0}{\svar_0}{\svalue_0}

& \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
& \snrlbl
& \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
\\[1.0ex]


\eappu{(\esfun{\svar_0}{\sshape_0}{\sexpr_0})}{\svalue_0}
& \snr
& \sscanerror

& \obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
& \snrlbl
& \obars{\sscanerror}{\sowner_1}
\\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
& \sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
\\[1.0ex]


\eappu{(\esfun{\svar_0}{\sshape_0}{\sexpr_0})}{\svalue_0}
& \snr
& \esubst{\sexpr_0}{\svar_0}{\svalue_0}

& \obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
& \snrlbl
& \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
\\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
& \sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
\\[1.0ex]


\eappu{(\emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0})}{\svalue_1}
& \snr
&

& \obars{\eappu{\obbars{\emon{(\tfun{\stype_0}{\stype_1})}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2}
& \snrlbl
\\[-2.0ex]\sidestep{\ewrap{\stype_1}{(\eappu{\svalue_0}{(\ewrap{\stype_0}{\svalue_1})})}\hspace{4mm}\hphantom{x}}
& \sidestep{\obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}}
\\[1.0ex]


\enoop{\svalue_0}
& \snr
& \svalue_0

& \obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1}
& \snrlbl
& \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
\\[1.0ex]


\escan{\sshape_0}{\svalue_0}
& \snr
& \sscanerror

& \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\sscanerror}{\sowner_1}
\\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
& \sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
\\[1.0ex]


\escan{\sshape_0}{\svalue_0}
& \snr
& \svalue_0

& \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
\\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
& \sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
\\[1.0ex]


\ewrap{\stype_0}{\svalue_0}
& \snr
& \swraperror

& \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\swraperror}{\sowner_1}
\\\sidecond{if $\neg\fshapematch{\fshape{\stype_0}}{\svalue_0}$}
& \sidecond{if $\neg\fshapematch{\fshape{\stype_0}}{\svalue_0}$}
\\[1.0ex]


\ewrap{(\tfun{\stype_0}{\stype_1})}{\svalue_0}
& \snr
& \emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0}

& \obars{\ewrap{(\tfun{\stype_0}{\stype_1})}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\emon{(\tfun{\stype_0}{\stype_1})}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
\\\sidecond{if $\fshapematch{\kfun}{\svalue_0}$}
& \sidecond{if $\fshapematch{\kfun}{\svalue_0}$}
\\[1.0ex]


\ewrap{(\tpair{\stype_0}{\stype_1})}{\epair{\svalue_0}{\svalue_1}}
& \snr
& \epair{\ewrap{\stype_0}{\!\svalue_0}}{\ewrap{\stype_1}{\!\svalue_1}}

& \obars{\ewrap{(\tpair{\stype_0}{\stype_1})}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\!\epair{\ewrap{\stype_0}{\!\obbars{\svalue_0}{\sownerlist_0}}\!\!}{\ewrap{\stype_1}{\!\obbars{\svalue_1}{\sownerlist_0}}}\!}{\sowner_1}\!
\\[1.0ex]


\ewrap{\stype_0}{\svalue_0}
& \snr
& \svalue_0

& \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
& \snrlbl
& \obars{\svalue_0}{\sowner_1}
\\\sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$}
& \sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$}

\\[2ex]

\multicolumn{3}{l}{\!\!\!\!\begin{tabular}{l@{~}l}
\fbox{\(\sexpr \srr \sexpr\)}\(~~\sdefeq\) & reflexive, transitive, compatible
\\ &  (w.r.t. $\sctx$) closure of $\snr$
\end{tabular}}
&
\multicolumn{3}{l}{\!\!\!\!\begin{tabular}{l@{~}l}
\fbox{\(\sexpr \srrlbl \sexpr\)}\(~~\sdefeq\) & reflexive, transitive, compatible
\\ &  (w.r.t. $\sctx$) closure of $\snrlbl$
\end{tabular}}
\end{array}\)
}|]


@figure[
  "fig:model:extra-rr"
  @elem{Semantic metafunctions}

@exact|{
\begin{minipage}[t]{0.48\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sunop\,}{\,\svalue}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\sfst, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_0
    \\
    \sdelta(\ssnd, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_1
  \end{langarray}
}
\end{minipage}\begin{minipage}[t]{0.52\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sbinop\,}{\tpair{\,\svalue\,}{\,\svalue}}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\ssum, \sint_0, \sint_1) & \feq & \sint_0 + \sint_1
    \\
    \sdelta(\squotient, \sint_0, 0) & \feq & \divisionbyzeroerror
    \\
    \sdelta(\squotient, \sint_0, \sint_1) & \feq & \floorof{\sint_0 / \sint_1}
  \end{langarray}
}
\end{minipage}

\lbl{\fbox{$\sshapematch : \ffun{\tpair{\sshape\,}{\,\svalue}}{\fbool}$}}{
  \begin{langarray}
   \fshapematch{\kfun}{\svalue_0} & \feq & \ftrue
   \\\sidecond{if $\svalue_0 \in \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \esfun{\svar}{\sshape}{\sexpr} \cup \emon{\stype}{\svalue}$}
   \\[0.5ex]
   \fshapematch{\kpair}{\epair{\svalue_0}{\svalue_1}} & \feq & \ftrue
   \\[0.5ex]
   \fshapematch{\kint}{\sint_0} & \feq & \ftrue
   \\[0.5ex]
   \fshapematch{\knat}{\snat_0} & \feq & \ftrue
   \\[0.5ex]
   \fshapematch{\kany}{\svalue_0} & \feq & \ftrue
   \\[0.5ex]
   \fshapematch{\sshape_0}{\svalue_0} & \feq & \ffalse
   \\\sidecond{otherwise}
  \end{langarray}
}
}|]


@section[#:tag "sec:model:model:reduction"]{Reduction Relation}

@Figure-ref{fig:model:rr} presents a notion of reduction for the evaluation
syntax.
Each rule @${(\sexpr \snr \sexpr)} in the figure relates two expressions.
Rules that share a syntactically-equal domain come with additional test
for the domain expression.
These tests use basic set theory to pattern-match on expressions;
for example, the test @${(\svalue_0 \in \efun{\svar}{\sexpr})} holds when
the value @${\svalue_0} is an unannotated lambda.

The reduction rules for unary and binary operations apply the @${\sdelta}
metafunction (@figure-ref{fig:model:extra-rr}) and halt with a tag error if
@${\sdelta} is undefined.
In general, @${\sdelta} models the behavior of a run-time system that works
at a lower level of abstraction than the evaluation language.
For unary operations, @${\sdelta} extracts an element from a pair.
For binary operations, @${\sdelta} performs arithmetic.

The rules for function application check that the first expression is a
function and typically substitute the argument expression into the function
body.
If the function has a type-shape annotation (@${\sshape}), then an additional
shape check validates the argument.
If the function is enclosed in a guard wrapper, then the application
unfolds into two @${\swrap} checks: one for the argument and one for
the result.
Functions that are wrapped in several guards must unfold an equal number of
times.

The remaining rules state the behavior of run-time checks.
A @|snoop| boundary performs no check and lets any value across.
A @|sscan| boundary checks the top-level shape of an incoming value against the
expected type-shape, and halts if the two disagree.
Lastly, a @|swrap| boundary checks the top-level shape of a value then goes
further based on the type.
For function types, a @${\swrap} installs a guard wrapper.
For pairs, a @${\swrap} validates both components and creates a new pair value.
For base types, the shape check is enough.

The overall semantics for the evaluation syntax is given by the reflexive,
transitive closure of the compatible closure of @${\snr} relative to the
evaluation contexts from @figure-ref{fig:model:eval-syntax}@~cite{fff-2009}.
Each expression has a unique redex thanks to the inductive structure of
evaluation contexts.



@section[#:tag "sec:model:model:ownership"]{Labeled Reduction Relation, @|sDeep| Label Consistency}

The model requires two final definitions to enable a syntactic analysis of
complete monitoring: a label-annotated reduction relation and a
consistency judgment that validates the labels.
Labels provide a specification of who owns what in a running program.
More precisely, the labels on an expression describe the surface
modules that are responsible for the behavior of the expression.
A consistently-labeled expression keeps @|sdeep|-typed code separate
from @|sshallow| and @|suntyped| code.
Informally, consistent labelling is possible if a semantics is able to chaperone
all inputs to and outputs from @|sdeep|-typed values.

@Figure-ref{fig:model:rrlbl} presents a labeled notion of reduction for the
evaluation language.@note{
The design of a labeled reduction relation is like any other
definition in that it requires ingenuity to create and careful reading
to understand.
To help readers gain an intuition about what makes for a good labeling,
the appendix presents the guidelines that the authors followed to create
@figure-ref{fig:model:rrlbl}.
}
By design, the reduction rules are identical to the basic rules from
@figure-ref{fig:model:rr} except for superscript labels and additional parentheses.
Labels are metadata; they do not change the underlying behavior of a reduction rule.
The labels on the left-hand expression of each rule give names to the
parties responsible for any relevant subexpressions.
The labels on the right-hand expression show how responsibilities change
in response to the reduction step.
For example, an untyped function application (@${\eappu{(\efun{\svar_0}{\sexpr_0})}{\svalue_0}})
substitutes an argument value into the function body.
Because of the substitution, the parties that were responsible for the
function become responsible for both the value and for the expression that
the function computes.
Labels typically accumulate.
The way that labels may disappear is after a successful run-time check
For example, the @${\swrap} rule for base types says that party @${\sowner_1} may
assume full responsibility of numbers that reach a well-typed boundary.

@figure[
  "fig:model:label-syntax"
  @elem{Labeled evaluation syntax}

@exact|{
\begin{langarray}
  \sexpr & \slangeq &
    \svar \mid \svalue \mid \epair{\sexpr}{\sexpr}
    \mid \eunop{\sexpr} \mid
    \ebinop{\sexpr}{\sexpr} \mid \eappu{\sexpr}{\sexpr} \mid \serror \mid
  \\ & &
    \ewrap{\stype}{\obars{\sexpr}{\sowner}}
    \mid \escan{\sshape}{\obars{\sexpr}{\sowner}}
    \mid \enoop{\obars{\sexpr}{\sowner}}
    \mid \obars{\sexpr}{\sowner}
  \\
  \svalue & \slangeq &
    \sint \mid \epair{\svalue}{\svalue}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \esfun{\svar}{\sshape}{\sexpr} \mid
  \\ & &
    \emon{\stype}{\obars{\svalue}{\sowner}}
    \mid \obars{\svalue}{\sowner}
  \\
  \sctx & \slangeq &
    \ldots \mid \obars{\sctx}{\sowner}
  \\
  \sowner & \slangeq &
    \sdowner_0 \mid \sdowner_1 \mid \ldots \mid
  %\\ & &
    \ssowner_0 \mid \ssowner_1 \mid \ldots \mid
  %\\ & &
    \suowner_0 \mid \suowner_1 \mid \ldots
  \\
  \sownerlist & \slangeq &
    \mbox{sequence of labels ($\sowner$)}
  \\
  \sownerenv & \slangeq &
    \cdot \mid \fcons{\tann{\svar}{\sowner}}{\sownerenv}
\end{langarray}

\medskip
Abbreviation: \(\obars{\cdots\obars{\sexpr_0}{\sowner_0}\cdots}{\sowner_n} = \obbars{\sexpr_0}{\fcons{\sowner_0}{\fcons{\cdots}{\sowner_n}}}\)
}|]

Technically, the addition of labels to the evaluation language calls for an
entirely new syntax.
@Figure-ref{fig:model:label-syntax} spells out the details.
The expression form @${\obars{\sexpr}{\sowner}} attaches a label to any
subexpression.
A similar value form @${\obars{\svalue}{\sowner}} lets any value appear
under an arbitrary number of labels.
Labels correspond to modules from the surface syntax, and thus combine
a kind (@${\sD}, @${\sS}, or @${\sU}) with a unique identifying number.
Beyond these straightforward changes, the labeled syntax has two noteworthy
aspects:
@itemlist[
@item{
All boundaries require a label for their subexpression.
This means that the @${\svalue_0} in the following four patterns must be
a labeled value: @${(\ewrap{\stype_0}{\svalue_0})},
@${(\escan{\sshape_0}{\svalue_0})}, @${(\enoop{\svalue_0})}, and @${(\emon{\stype_0}{\svalue_0})}.
@linebreak[]}
@item{
To minimize parenthesis and superscripts, the abbrevation @${\obbars{\cdot}{\cdot}}
captures a sequence of labels.
For example, the value @${\obars{\obars{\obars{4}{\sowner_0}}{\sowner_1}}{\sowner_2}}
matches the pattern @${\obbars{\svalue_0}{\sownerlist_0}}
with @${\svalue_0\!=\!42}
and @${\sownerlist_0\!=\!\fcons{\sowner_0}{\fcons{\sowner_1}{\sowner_2}}}.
}
]

@Figure-ref{fig:model:ownership-consistency} presents a consistency
judgment for labeled expressions.
The judgment allows any mix of @|sshallow| (@${\sS}) and @|suntyped| (@${\sU})
labels around an expression, but restricts the use of @|sdeep| (@${\sD}) labels.
Concretely, the judgment analyzes an expression relative to a context label
and an environment (@${\sownerenv}).
Any variables must have a binding in the label environment that matches the
context label.
Most expressions simply need consistent subterms.
Boundary expressions and guards values are switch points; these terms
are consistent if their subterm matches the context label that appears
inside the boundary.
Lastly, the rules for labeled expressions specify the allowed mixtures.
A @|sdeep|-labeled expression may have other @|sdeep| labels, but nothing else.
@|sShallow| and @|suntyped|-labeled expressions can mix together.
Hence the name @emph{@|sdeep|-label consistency} describes what the judgment
checks for.


@figure*[
  "fig:model:ownership-consistency"
  @elem{@|sDeep| label consistency}

@exact|{
\begin{minipage}{\textwidth}
\lbl{\fbox{\(\sowner; \sownerenv \sWL \sexpr\)}}{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sowner_0} \in \sownerenv_0
  }{
    \sowner_0; \sownerenv_0 \sWL \svar_0
  }

  \inferrule*{
  }{
    \sowner_0; \sownerenv_0 \sWL \sint_0
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \epair{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\svar_0}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \esfun{\svar_0}{\sshape_0}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \eunop{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \ebinop{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \eappu{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
  }{
    \sowner_0; \sownerenv_0 \sWL \serror
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \enoop{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \escan{\sshape_0}{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \ewrap{\stype_0}{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \svalue_0
  }{
    \sowner_0; \sownerenv_0 \sWL \emon{\stype_0}{\obars{\svalue_0}{\sowner_1}}
  }

  \inferrule*{
    \sdowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sdowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\sdowner_1}
  }

  \inferrule*{
    \ssowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\ssowner_1}
  }

  \inferrule*{
    \suowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\suowner_0}
  }

  \inferrule*{
    \suowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\suowner_1}
  }

  \inferrule*{
    \ssowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\ssowner_0}
  }

\end{mathpar}
}
\end{minipage}
}|]



@section[#:tag "sec:model:model:theorems"]{Properties}

The time has come to validate the model's claim that it allows safe interactions
among @|sdeep|-typed code, @|sshallow|-typed code, and @|suntyped| code.
Cleary the surface language defines three kinds of code and
the evaluation language lets differently-typed code interact,
but it remains to be seen whether the mixed language satisfies the properties that
characterize @|sdeep| and @|sshallow| types.
@|sDeep| code should provide a strong type soundness guarantee and
complete monitoring.
@|sShallow| code should satisfy a shape-level type soundness guarantee.

@;Naturally, these outcomes depend on the ``strength'' of the static types;
@; for example, @|suntyped| code has weaker guarantees than @|sshallow| code.
@;Complete monitoring asks whether single-owner consistency is an invariant;
@; if so, then programmers can trust @|sdeep| types as behavioral guarantees.

Type soundness predicts the possible outcomes of a well-typed expression.
Because the surface language allows three kinds of typed expression
(@|sdeep|, @|sshallow|, and @|suntyped|), the following statement of type
soundness is parameterized over both a language kind @${\slang} and a characterization
function @${\stypemap : \ffun{\stspec}{(\stype \cup \sshape \cup \tdyn)}\,}
of possible outcomes.

The notation @${\ssurface_0 \srr \sexpr_0} defines evaluation for surface
 expressions.
The meaning is that @${\ssurface_0} is well-typed
 (@${\fexists{\stspec}{\sST \ssurface_0 : \stspec}}) and
 compiles to an intermediate evaluation expression (@${\sST \ssurface_0 : \stspec \scompile \sexpr_1})
 that eventually reduces to the named expression (@${\sexpr_1 \srr \sexpr_0}).

@exact|{
\begin{definition}[$\fTS{\slang}{\stypemap}$]
  Language\ $\slang$
  satisfies\ $\fTS{\sWTlang}{\stypemap}$
  if for all\ $\ssurface_0$
  such that\ $\sST \ssurface_0 : \stspec$
  holds, one of the following holds:
  \begin{itemize}
    \item $\ssurface_0 \srr \svalue_0$ and\ $\sWTlang \svalue_0 : \ftypemap{\stspec}$
    \item $\ssurface_0 \srr \serror$
    \item $\ssurface_0 \srr$ diverges
  \end{itemize}
\end{definition}
}|

@; @|noindent|Note that @${\sTS} does not rule out any particular errors.
@; Two methods could enable a finer statement:
@;  (1) split the evaluation-language notion of reduction into three notions
@;  and introduce new errors to distinguish invariant failures from allowed tag errors;
@;  or (2) introduce three kinds of evaluation context
@;  and show that reductions in typed code do not raise tag errors.
@; @citet{gdf-draft-2020} demonstrate the first method.
@; @citet{gf-icfp-2018} demonstrate the second.

There are three important characterization functions @${\stypemap} for the analysis:
 @${\stypemapzero} maps every surface type to @${\tdyn};
 @${\stypemapshape} maps types to shapes (same as @${\sshapecheck} from @figure-ref{fig:model:shallow-type})
 and the unitype @${\tdyn} to itself;
 and @${\stypemapone} is the identity function on types and @${\tdyn}.

@exact|{
\begin{theorem}[type soundness]\leavevmode
  \begin{itemize}
    \item Language\ $\sU$ satisfies\ $\fTS{\sWTU}{\stypemapzero}$
    \item Language\ $\sS$ satisfies\ $\fTS{\sWTS}{\stypemapshape}$
    \item Language\ $\sD$ satisfies\ $\fTS{\sWTD}{\stypemapone}$
  \end{itemize}
\end{theorem}
\begin{proofsketch}
 By three lemmas for progress, preservation, and compilation (deferred to the appendix).
 The compilation lemma says that every well-typed surface expression compiles
 to a well-typed evaluation expression.
\end{proofsketch}
}|

Complete monitoring states that the evaluation language has control
over every interaction between @|sdeep|-typed code and weaker code.
More precisely, the question is whether the labels
that arise in evaluation are consistent according to the @${\sWL} judgment
(@figure-ref{fig:model:ownership-consistency}).
Both @${\sexpr_0} and @${\sexpr_1} below refer to labeled expressions.
@; if no labeling for (compiled s0) exists, then vacuously true, victory

@exact|{
\begin{theorem}[complete monitoring]
  If\ $~\sST \ssurface_0 : \stspec$
  and\ $\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and\ $\sowner_0; \cdot \Vdash \sexpr_0$
  and\ $\sexpr_0 \srr \sexpr_1$
  then\ $\sowner_0; \cdot \Vdash \sexpr_1$.
\end{theorem}
\begin{proofsketch}
  By a preservation argument.
  The proofs for each basic reduction step are sketched below.
  These depend on two metafunctions: $\srev$ reverses a sequence of labels
   and $\slast$ extracts the last (outermost) element of such a sequence.

  \begin{description}
  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition, \(\sowner_1; \cdot \sWL \obars{\stagerror}{\sowner_1}\)

  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}\)
    \begin{enumerate}
    \item
      $\sownerlist_0$ is either all \sdeep{} labels or a mix of \sshallow{} and \suntyped{}, by \sdeep{}-label consistency of the redex
    \item
      similarly, $\sowner_1$ must match $\sownerlist_0$
    \item
      $\svalue_0$ is a pair, because $\sdelta$ is defined on it
    \item
      both components of $\svalue_0$ are well-labeled, again by \sdeep{}-label consistency on the redex
    \item
      by the definition of $\sdelta$
    \end{enumerate}

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\stagerror}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$ and $\sdelta$; note that the binary operators are not elimination forms in the model, thus the number
    computed by $\sdelta$ does not acquire the labels on $\svalue_0$ and $\svalue_1$

  \item[Case:]
    \(\obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrbreak \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
    \begin{enumerate}
    \item\label{step:model:cm:1}
      $\sownerlist_0$ is all \sdeep{} or a mix of \sshallow{} and \suntyped{}, by \sdeep{}-label consistency of the redex
    \item\label{step:model:cm:2}
      $\sowner_2; \cdot \sWL \svalue_0$, also by \sdeep{}-label consistency of the redex
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}$, by steps~\ref{step:model:cm:1} and~\ref{step:model:cm:2}
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \svar_0$ for each occurrence of $\svar_0$ in $\sexpr_0$, by \sdeep{}-label consistency of the redex
    \item
      by a substitution lemma
    \end{enumerate}

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrbreak \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the previous case

  \item[Case:]
    \(\obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrbreak \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the other substitution cases

  \item[Case:]
    \(\obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2} \snrbreak
      \obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)
    \begin{enumerate}
    \item
      $\sowner_0; \cdot \sWL \svalue_0$, by \sdeep{}-label consistency of the redex
    \item
      $\sowner_2; \cdot \sWL \svalue_1$, again by \sdeep{}-label consistency of the redex
    \item
      $\sownerlist_1$ is either all \sdeep{} or a mix of \sshallow{} and \suntyped{}, again by the redex
    \item
      by the definition of $\sWL$
    \end{enumerate}

  \item[Case:]
    \(\obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\snoop{}$ boundary connects either:
     two \sdeep{} components, two \sshallow{} components, two \suntyped{} components, or one \sshallow{} and one \suntyped{} component

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\sscan{}$ boundary only links an \suntyped{} component to a \sshallow{} component

  \item[Case:]
    \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\swraperror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ewrap{(\tfun{\stype_0}{\stype_1})}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ewrap{(\tpair{\stype_0}{\stype_1})}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1} \snrbreak
      \obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$;
    note that the rule moves the elements of the pair in the redex into a new pair in the contractum

  \item[Case:]
  \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\svalue_0}{\sowner_1}\)
  \\ where $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$ 
  \item[]
    by the definition of $\sWL$

  \end{description}
\end{proofsketch}
}|


