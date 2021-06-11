#lang scribble/acmart @acmsmall @10pt @screen
@(require "main.rkt" "bib.rkt"
   (only-in "pict.rkt" fig:model-interaction))

@; THESIS Deep and Shallow can interoperate
@; - don't worry about CM extension, for self-edges and S/U
@;   - (2021-04-25) that means, don't present the self-loops? or don't focus on them?
@; - don't worry about false-start designs ... point to dissertation for now

@title[#:tag "sec:model"]{Model and Metatheory}

A typical mixed-typed language allows for two kinds of code, typed and untyped,
and employs run-time checks to protect the claims made by static types.
The model of this section goes further by allowing three variants:
@|sdeep|-typed code, @|sshallow|-typed code, and @|suntyped| code.
Both the @|sdeep| and @|sshallow| code must satisfy a type checker that
validates typical well-formedness properties.
The @|suntyped| code has fewer constraints, but is nevertheless free to
communicate with @|sdeep| and @|sshallow| chunks of code.
All three syntaxes compile to an evaluation language that uses run-time
checks to mediate any interactions.

Overall, the primary goal of the model is to test whether @|sdeep|, @|sshallow|,
and @|suntyped| code can safely interoperate.
The @|sdeep| types must satisfy type soundness and complete monitoring properties,
and the @|sshallow| types must satisfy only a weak type soundness.
A secondary goal of the model is to serve as the outline for an implementation.
For this reason, the three syntaxes compile to @emph{one} kernel language
that uses standard run-time-checking concepts.
A @emph[swrap] term corresponds to a higher-order contract,
a @emph[sscan] performs a first-order check,
and a @emph[snoop] does nothing.
@Sectionref{sec:model:model:theorems} proves the main result of this section;
namely, that a careful use of these checks
can ensure safe, three-dimensional interactions.


@figure*[
  "fig:model:base-interaction"
  @elem{@|sDeep|, @|sShallow|, and @|suntyped| interactions.}
  fig:model-interaction]


@section[#:tag "sec:model:model:syntax"]{Three-way Surface Syntax}

@figure*[
  "fig:model:surface"
  @elem{Surface syntax}

  @exact|{
\begin{langarray}
  \ssurface & \slangeq &
    \svar \mid \sint \mid \epair{\ssurface}{\ssurface} \mid
  \\ & &
    \efun{\svar}{\ssurface}
    \mid \efun{\tann{\svar}{\stype}}{\ssurface}
    \mid \efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface} \mid
  \\ & &
    \eunop{\ssurface} \mid \ebinop{\ssurface}{\ssurface} \mid
    \eappu{\ssurface}{\ssurface} \mid
  \\ & &
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
Expressions @${\ssurface} consist of function applications (@${\eappu{\ssurface}{\ssurface}}),
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
 integers @${\tint},
 pairs @${\tpair{\stype}{\stype}},
 and functions (@${\tfun{\stype}{\stype}}).
Modules associate a label with an expression (@${\emod{\slang}{\ssurface}}).
The label @${\slang} is either @${\sD} for @|sdeep|-typed code,
 @${\sS} for @|sshallow|-typed code, or @${\sU} for @|suntyped| code.
For example, the term @${(\emod{\sD}{\ssurface_0})} says that @${\ssurface_0}
is a @|sdeep|-typed expression.
Any module expressions within @${\ssurface_0} can
be @|sdeep|-typed, @|sshallow|-typed, or @|suntyped|.


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
expression @${\ssurface_0} via a module boundary.
The full expression @${\ssurface_0} satisfies the typing judgment if the
@|sdeep| parts are well-typed and the @|suntyped| parts are well-formed.

@Figureref{fig:model:extra-type} defines a subtyping judgment (@${\ssubt}
and a type-assignment for primitive operations (@${\sDelta}).
These are both standard.
Subtyping says that natural numbers are valid integers, and is covariant
for pairs and contravariant for functions.
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



@figure*[
  "fig:model:surface-type"
  @elem{Surface typing judgment (selected rules, others in @appendixref{appendix:rules})}

@exact|{
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
    \stypeenv \sST \sint_0 : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv \sST \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv \sST \snat_0 : \tfloor{\tnat}
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tint
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tfloor{\tint}
  }

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
%    \stypeenv \sST \eapp{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv \sST \sexpr_0 : \tfun{\stype_0}{\stype_1}
%    \\
%    \stypeenv \sST \sexpr_1 : \stype_0
%  }{
%    \stypeenv \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_1
%  }
%
  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_0}{\stype_1}}
    \\
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
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn
  }{
    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
  }{
    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn
  }{
    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
  }{
    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn
  }{
    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \tfloor{\stype_0}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
  }{
    \stypeenv \sST \emodule{\sdlang}{\sexpr_0} : \tfloor{\stype_0}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \tfloor{\stype_0}
  }
\end{mathpar}
}|]

@figure*[
  "fig:model:extra-type"
  @elem{Subtyping judgment and types for primitive operations.}

  @exact|{
  \begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\fsubt{\stype}{\stype}$}}{\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\tnat}{\tint}
  }

  \inferrule*{
    \fsubt{\stype_0}{\stype_2}
    \\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tpair{\stype_0}{\stype_1}}{\tpair{\stype_2}{\stype_3}}
  }

  \inferrule*{
    \fsubt{\stype_2}{\stype_0}
    \\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tfun{\stype_0}{\stype_1}}{\tfun{\stype_2}{\stype_3}}
  }
\end{mathpar}}
\end{minipage}%
\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sDelta : \ffun{\tpair{\sunop\,}{\stype}}{\stype}$}}{
  \begin{langarray}
    \sDelta(\sfst, \tpair{\stype_0}{\stype_1}) & \feq & \stype_0
  \\
    \sDelta(\ssnd, \tpair{\stype_0}{\stype_1}) & \feq & \stype_1
  \end{langarray}
}

\lbl{\fbox{$\sDelta : \ffun{\tpair{\sbinop\,}{\tpair{\stype}{\stype}}}{\stype}$}}{
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


@section[#:tag "sec:model:model:eval-syntax"]{Common Evaluation Syntax}

By contrast to the declarative surface syntax, the purpose of the evaluation
syntax is to present a core set of expressions that can support three-way
interactions.
The syntax removes surface terms that merely express an intent and adds
terms for run-time checks.

Evaluation expressions @${\sexpr} consist of variables, values, primitive
applications, function applications, errors, and boundary terms.
The module boundaries from the surface syntax are gone.
Instead, three @emph{boundary terms} directly suggest run-time checks.
A @|swrap| boundary asks for the full enforcement of a type, either with a comprehensive first-order
check or a higher-order wrapper;
a @|sscan| boundary asks for a first-order type-shape (@${\sshape}) check;
and a @|snoop| boundary asks for no check.

Together, values and errors represent the possible results of an evaluation.
A value is either an integer, a pair, a function, or a guard wrapper.
A guard @${(\emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0})} is a restricted
function; it provides access to the function @${\svalue_0} subject to run-time
checks based on the @${(\tfun{\stype_0}{\stype_1})} type.
Note also that @|sshallow|-typed functions have a shape annotation in
the evaluation syntax (@${\efun{\tann{\svar}{\sshape}}{\sexpr}}) rather
than an underlined type annotation (@${\efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface}}).
An error may arise from either a failed check at @|swrap| boundary (@${\swraperror}),
a failed check at a @|sscan| boundary (@${\sscanerror}), a division by zero
(@${\sdivzeroerror}), or a malformed untyped expression (@${\stagerror}).

@; other notes 2021-06-08
@; - shape-ann on functions reflects runtime knowledge ... and also the dom-check to insert
@; - example tag error = app 3 4


@figure*[
  "fig:model:eval-syntax"
  @elem{Evaluation Syntax}

  @exact|{
\begin{langarray}
  \sexpr & \slangeq &
    \svar \mid \svalue \mid \epair{\sexpr}{\sexpr}
    \mid \eunop{\sexpr} \mid
    \ebinop{\sexpr}{\sexpr} \mid \eappu{\sexpr}{\sexpr} \mid \serror \mid
  \\ & &
    \ewrap{\stype}{\sexpr}
    \mid \escan{\sshape}{\sexpr}
    \mid \enoop{\sexpr}
  \\
  \svalue & \slangeq &
    \sint \mid \epair{\svalue}{\svalue}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \efun{\tann{\svar}{\sshape}}{\sexpr}
    \mid \emon{(\tfun{\stype}{\stype})}{\svalue}
  \\
  \sshape & \slangeq &
    \knat \mid \kint \mid \kpair \mid \kfun \mid \kany
  \\
  \serror & \slangeq &
    \swraperror \mid \sscanerror \mid \sdivzeroerror \mid \stagerror
  \\
  \sctx & \slangeq &
    \sctxhole \mid \eunop{\sctx} \mid \ebinop{\sctx}{\sexpr} \mid \ebinop{\svalue}{\sctx}
    \mid \eappu{\sctx}{\sexpr} \mid \eappu{\svalue}{\sctx} \mid
  \\ & &
    \enoop{\sctx} \mid \escan{\sshape}{\sctx} \mid \ewrap{\stype}{\sctx}
\end{langarray}
}|]


@section[#:tag "sec:model:model:eval-types"]{Three-way Evaluation Typing}

The evaluation syntax comes with three typing judgments that describe the
run-time invariants of @|sdeep|, @|sshallow|, and @|suntyped| code.
The @|sdeep| typing judgment (@${\sWTD}) validates full types.
The @|sshallow| judgment (@${\sWTS}) checks top-level type shapes.
Lastly, the @|suntyped| judgment (@${\sWTU}) checks that all variables
have proper bindings.

Both the @|sdeep| and @|suntyped| rules are similar to the corresponding
surface-language rules because they support equally-strong conclusions
(full types and the unitype).
The @|sshallow| judgment is rather different because it validates type
shapes instead of full (underlined) types.
When inspecting a pair, for example, the judgment concludes with the @${\kpair}
shape no matter what shapes the elements have.
Consequently, a pair elimination form such as @${(\efstu{\svar_0})} has the
@${\kany} shape because the pair may contain any sort of value.
Similar comments apply to functions and applications.
Thus, if a program expects a particular shape from the element of a pair or the
range of a function, then the program must use a @${\sscan} assertion to
check the expected shape.

@figure*[
  "fig:model:deep-type"
  @elem{@|sDeep| typing judgment}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\stype_0} \in \stypeenv
  }{
    \stypeenv \sWTT \svar_0 : \stype_0
  }

  \inferrule*{
  }{
    \stypeenv \sWTT \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv \sWTT \sint_0 : \tint
  }

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
    \\
    \stypeenv \sWTT \sexpr_1 : \stype_1
  }{
    \stypeenv \sWTT \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1}
  }

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

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
    \\\\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv \sWTT \eunop{\sexpr_0} : \stype_1
  }

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
    \\
    \stypeenv \sWTT \sexpr_1 : \stype_1
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv \sWTT \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
  }

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

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sWTT \sexpr_0 : \stype_1
  }

  \inferrule*{
  }{
    \stypeenv \sWTT \serror : \stype_0
  }
\end{mathpar}
}|]

@figure*[
  "fig:model:shallow-type"
  @elem{@|sShallow| typing judgment, subtyping, and shape map}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sshape_0} \in \stypeenv
  }{
    \stypeenv \sWTS \svar_0 : \sshape_0
  }

  \inferrule*{
  }{
    \stypeenv \sWTS \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv \sWTS \sint_0 : \tint
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
    \\
    \stypeenv \sWTS \sexpr_1 : \sshape_1
  }{
    \stypeenv \sWTS \epair{\sexpr_0}{\sexpr_1} : \kpair
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTS \efun{\svar_0}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTS \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \stypeenv \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv \sWTS \emon{\stype_0}{\svalue_0} : \kfun
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv \sWTS \eunop{\sexpr_0} : \kany
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
    \\
    \stypeenv \sWTS \sexpr_1 : \sshape_1
    \\
    \sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2
  }{
    \stypeenv \sWTS \ebinop{\sexpr_0}{\sexpr_1} : \sshape_2
  }

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
    %% TODO why need this???
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

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
    \\
    \fsubt{\sshape_0}{\sshape_1}
  }{
    \stypeenv \sWTS \sexpr_0 : \sshape_1
  }

  \inferrule*{
  }{
    \stypeenv \sWTS \serror : \sshape_0
  }
\end{mathpar}

\bigskip
\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\fsubt{\sshape}{\sshape}$}}{\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\knat}{\kint}
  }

  \inferrule*{
  }{
    \fsubt{\sshape_0}{\kany}
  }
\end{mathpar}}
\end{minipage}%
\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sshapecheck : \ffun{\stype}{\sshape}$}}{
  \begin{langarray}
    \fshape{\tnat} & \feq & \knat
  \\
    \fshape{\tint} & \feq & \kint
  \\
    \fshape{\tpair{\stype_0}{\stype_1}} & \feq & \kpair
  \\
    \fshape{\tfun{\stype_0}{\stype_1}} & \feq & \kfun
  \end{langarray}
}
\end{minipage}
}|]


@figure*[
  "fig:model:untyped-type"
  @elem{Untyped typing judgment}
  @; ... aka dynamic typing, most types checked at runtime

@exact|{
\bigskip
\bigskip
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\tdyn} \in \stypeenv
  }{
    \stypeenv \sWTU \svar_0 : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv \sWTU \sint_0 : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv \sWTU \epair{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTU \efun{\svar_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTU \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv \sWTU \emon{\stype_0}{\svalue_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTU \eunop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv \sWTU \ebinop{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv \sWTU \eappu{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTU \enoop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv \sWTU \enoop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv \sWTU \ewrap{\stype_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv \sWTU \serror : \tdyn
  }
\end{mathpar}
}|]

@figure*[
  "fig:model:completion1"
  @elem{Surface-to-evaluation completion (selected rules, others in @appendixref{appendix:rules})}

@exact|{
\begin{mathpar}
  \inferrule*{
  }{
    \stypeenv \sST \svar_0 : \tdyn \scompile \svar_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \svar_0 : \stype_0 \scompile \svar_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \svar_0 : \tfloor{\stype_0} \scompile \svar_0
  }

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
    \stypeenv \sST \efun{\tann{\svar_0}{\tfloor{\stype_0}}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}} \scompile \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \tdyn \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfun{\stype_1}{\stype_0} \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \stype_0 \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_1}{\stype_0}} \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv \sST \eappu{\sexpr_0}{\sexpr_1} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{(\eappu{\sexpr_2}{\sexpr_3})}
  }

\end{mathpar}
}|]

@; TODO present as table
@figure*[
  "fig:model:completion2"
  @elem{Completion for module boundaries. Recall @figure-ref{fig:model:base-interaction}.}

@exact|{
\begin{tabular}{l@{\qquad}l}
\(\left[
  \inferrule*[width=4cm]{
    \stypeenv \sST \sexpr_0 : \stspec_0 \scompile \sexpr_1
  }{
    \stypeenv \sST \emodule{\slang_0}{\sexpr_0} : \stspec_1 \scompile \sexpr_2
  }
\right]\) &
\(\begin{array}{llll}
  \slang_0 & \stspec_0 & \stspec_1 & \scompile \sexpr_2 \\\hline
  \sU & \tdyn & \tdyn & \enoop{\sexpr_1} \\
  \sS & \tfloor{\stype_0} & \tdyn & \enoop{\sexpr_1} \\
  \sD & \stype_0 & \stype_0 & \enoop{\sexpr_1} \\
  \sS & \tfloor{\stype_0} & \tfloor{\stype_0} & \enoop{\sexpr_1} \\[1ex]
  \sU & \tdyn & \tfloor{\stype_0} & \escan{\sexpr_1} \\[1ex]
  \sD & \stype_0 & \tdyn & \ewrap{\sexpr_1} \\
  \sU & \tdyn & \stype_0 & \ewrap{\sexpr_1} \\
  \sS & \tfloor{\stype_0} & \stype_0 & \ewrap{\sexpr_1} \\
  \sD & \stype_0 & \tfloor{\stype_0} & \ewrap{\sexpr_1}
\end{array}\)
\end{tabular}

}|]


@section[#:tag "sec:model:model:completion"]{Compilation from Surface to Evaluation}

The surface syntax has no semantics of its own.
Instead, a compilation pass maps surface terms to simpler evaluation-language
terms which state exactly which run-time checks to perform.
Most checks appear at boundaries; indeed, the main task of compilation is
to replace surface module boundaries with check boundaries.
Additional checks appear is @|sshallow|-typed code to support its type
soundness property.

Because compilation inserts run-time checks and little more, it is more
like a completion pass that makes implicit operations explicit@~cite{h-scp-1994}
than a typical compiler.
Henceforth, this paper uses the term @emph{completion} instead of compilation.

The overall goal of completion-inserted checks is to map all well-typed surface
expressions to well-typed evaluation expressions
(@exact{\lemmaref{lemma:model:completion}}).
@itemlist[
@item{
 In @|sdeep|-typed code, completion inserts @|swrap| expressions at the
  module boundaries to less-typed code.
 Other @|sdeep| expressions have no checks.
}
@item{
 In @|sshallow| code, completion scans incoming untyped code and the result
  of every elimination form.
}
@item{
 In untyped code, completion adds no run-time checks.
 At the boundaries to @|sdeep| and @|sshallow| code, however, the above
  strategies call for a @|swrap| or @|sscan| check.
}
]
@|noindent|The rules shown in @figureref["fig:model:completion2" "fig:model:completion1"]
say exactly how to insert the checks.
@Figure-ref{fig:model:completion2} presents the rules for module boundaries
using a tabular notation.
The parameterized judgment at the left of the figure summarizes the rules for
boundaries, and the table at the right shows the specific parameters for the
nine possible combinations.
These nine rules correspond to the six edges in
@figure-ref{fig:model:base-interaction} plus three self-edges.

@Figureref{fig:model:completion1} illustrates the completion rules for
functions.
In @|sdeep|-typed and @|suntyped| code, the completion of an application is simply
the completion of its subexpressions.
In @|sshallow|-typed code, this elimination form requires a @|sscan| check to
validate the result.
Pairs and pair elimination forms follow a similar pattern.
The completion rules for other expressions simply transform their subexpressions
and are deferred to an appendix.

@; TODO sounds like a great improvement!!!
@; what about 2 kinds of shallow function? (\(x:o) ...) (\(scan x:o)[check] ...) ?
@bold{Note}: The completion of a @|sshallow| function is very simple---to the point of
being misleading---because the evaluation syntax has no way to represent domain
checks.
In a realistic language, @|sshallow| functions must translate to an un-annotated
function that first @|sscan|s the shape of its input and then proceeds with the
body expression.
The model does not show the domain check, and instead expects the underlying
semantics to always @${\sscan} the inputs of @|sshallow| functions (@sectionref{sec:model:model:reduction}).
Although this baked-in design simplifies the model and proof details regarding
substitution, the lack of an explicit domain check means that the model cannot
support a pass that eliminates redundant checks.



@figure*[
  "fig:model:rr"
  @elem{Semantics for the evaluation syntax}

@exact|{
\begin{rrarray}
  \eunop{\svalue_0} & \snr
  & \stagerror
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is undefined}
  \\[1.0ex]
  \eunop{\svalue_0} & \snr
  & \sdelta(\sunop, \svalue_0)
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
  \\[1.0ex]
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \stagerror
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
  \\[1.0ex]
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \sdelta(\sbinop, \svalue_0, \svalue_1)
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
  \\[1.0ex]
  \eappu{\svalue_0}{\svalue_1} & \snr &
  \stagerror
  \\\sidecond{if $\svalue_0 \not\in \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
  \\[1.0ex]
  \eappu{(\efun{\svar_0}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \sscanerror
  \\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \eappu{(\emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0})}{\svalue_1} & \snr
  & \ewrap{\stype_1}{(\eappu{\svalue_0}{(\ewrap{\stype_0}{\svalue_1})})}
  \\[1.0ex]
  \enoop{\svalue_0} & \snr
  & \svalue_0
  \\[1.0ex]
  \escan{\sshape_0}{\svalue_0} & \snr
  & \sscanerror
  \\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \escan{\sshape_0}{\svalue_0} & \snr
  & \svalue_0
  \\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\stype_0}{\svalue_0} & \snr
  & \swraperror
  \\\sidecond{if $\neg\fshapematch{\fshape{\stype_0}}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{(\tfun{\stype_0}{\stype_1})}{\svalue_0} & \snr
  & \emon{(\tfun{\stype_0}{\stype_1})}{\svalue_0}
  \\\sidecond{if $\fshapematch{\kfun}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{(\tpair{\stype_0}{\stype_1})}{\epair{\svalue_0}{\svalue_1}} & \snr
  & \epair{\ewrap{\stype_0}{\svalue_0}}{\ewrap{\stype_1}{\svalue_1}}
  \\[1.0ex]
  \ewrap{\stype_0}{\svalue_0} & \snr
  & \svalue_0
  \\\sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$}
\end{rrarray}

\medskip
\lbl{\fbox{\(\sexpr \srr \sexpr\)}\(~~\sdefeq \mbox{reflexive, transitive, compatible (w.r.t. $\sctx$) closure of $\snr$}\)}{
}

}|]


@figure*[
  "fig:model:extra-rr"
  @elem{Semantic metafunctions}

@exact|{
\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sunop}{\svalue}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\sfst, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_0
    \\
    \sdelta(\ssnd, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_1
  \end{langarray}
}

\end{minipage}\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sbinop}{\tpair{\svalue}{\svalue}}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\ssum, \sint_0, \sint_1) & \feq & \sint_0 + \sint_1
    \\
    \sdelta(\squotient, \sint_0, 0) & \feq & \divisionbyzeroerror
    \\
    \sdelta(\squotient, \sint_0, \sint_1) & \feq & \floorof{\sint_0 / \sint_1}
  \end{langarray}
}
\end{minipage}

\lbl{\fbox{$\sshapematch : \ffun{\tpair{\sshape}{\svalue}}{\fbool}$}}{
  \begin{langarray}
   \fshapematch{\kfun}{\svalue_0} & \feq & \ftrue
   \\\sidecond{if $\svalue_0 \in \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
   \\[0.8ex]
   \fshapematch{\kpair}{\epair{\svalue_0}{\svalue_1}} & \feq & \ftrue
   \\[0.8ex]
   \fshapematch{\kint}{\sint_0} & \feq & \ftrue
   \\[0.8ex]
   \fshapematch{\knat}{\snat_0} & \feq & \ftrue
   \\[0.8ex]
   \fshapematch{\kany}{\svalue_0} & \feq & \ftrue
   \\[0.8ex]
   \fshapematch{\sshape_0}{\svalue_0} & \feq & \ffalse
   \\\sidecond{otherwise}
  \end{langarray}
}
}|]

@section[#:tag "sec:model:model:reduction"]{Reduction Relation}

@Figure-ref{fig:model:rr} presents a notion of reduction for the evaluation
syntax.
Each rule @${(\sexpr \snr \sexpr)} in the figure relates two expressions.
Rules that share a common domain additionally come with a test to disambiguate.
These tests often use basic set theory to pattern-match on expressions;
for example, the test @${(\svalue_0 \in \efun{\svar}{\sexpr})} holds when
the value @${\svalue_0} is an unannotated lambda.
The rules also depend on two metafunctions, @${\sdelta} and @${\sshapematch},
that are defined in @figure-ref{fig:model:extra-rr}.

The reduction rules for unary and binary operations apply the @${\sdelta}
metafunction and halt with a tag error if @${\sdelta} is undefined.
In general, @${\sdelta} models the behavior of a run-time system that works
at a lower level of abstraction than the evaluation language.
For unary operations, @${\sdelta} extracts an element from a pair.
For binary operations, @${\sdelta} performs arithmetic.

The rules for function application check that the first expression is a
function and typically substitute the argument expression into the function
body.
If the function has a type-shape annotation (@${\sshape}), then an additional
shape check validates the argument.
And if the function is enclosed in a guard wrapper, then the application
unfolds into two @${\swrap} checks: one for the argument and one for
the result.
Functions that are wrapped in several guards must unfold an equal number of
times.

The remaining rules state the behavior of run-time checks.
A @|snoop| boundary performs no check and lets any value across.
A @|sscan| boundary checks the top-level shape of an incoming value against the
expected type-shape, and halts if the two disagree.
Lastly, a @|swrap| boundary checks the top-level shape of a value then proceeds
based on the type.
For function types, a @${\swrap} installs a guard wrapper.
For pairs, a @${\swrap} validates both components and creates a new pair value.
For base type, a shape check is enough for a comprehensive run-time check.

The overall semantics for the evaluation syntax is given by the reflexive,
transitive closure of the compatible closure of @${\snr} relative to the
evaluation contexts from @figure-ref{fig:model:eval-syntax}@~cite{fff-2009}.
Note that these left-to-right contexts are such that each expression has
a unique redex.


@section[#:tag "sec:model:model:theorems"]{Properties}

The primary meta-theoretic results are about type soundness and
 complete monitoring.
Type soundness predicts the possible outcomes of a well-typed expression.
Naturally, these outcomes depend on the ``strength'' of the static types;
 for example, @|suntyped| code has weaker guarantees than @|sshallow| code.
Complete monitoring asks whether single-owner consistency is an invariant;
 if so, then programmers can trust @|sdeep| types as behavioral guarantees.

The statement of type soundness relies on one new notation and a family of
 metafunctions.
The notation @${\ssurface_0 \srr \sexpr_0} defines evaluation for surface
 expressions; the meaning is that @${\ssurface_0} is well-typed somehow
 (@${\fexists{\stspec}{\sST \ssurface_0 : \stspec}}),
 compiles to an evaluation expression (@${\sST \ssurface_0 : \stspec \scompile \sexpr_1}),
 and then the compiled expression steps to the result (@${\sexpr_1 \srr \sexpr_0}).
The metafunctions---@${\stypemapzero}, @${\stypemapshape}, and @${\stypemapone}---map
 surface-language types to evaluation types.
One function, @${\stypemapshape}, extends the similarly-name function from
 @figureref{fig:model:shallow-type} to map the unitype @${\tdyn} to itself.
The others are simple: @${\stypemapzero} maps all types to @${\tdyn}
 and @${\stypemapone} is the identity.
These tools enable a concise, parameterized statement of type soundness.

Note that type soundness does not rule out any particular errors.
Two extensions could enable a finer statement:
 (1) split the one notion of reduction into three and introduce new errors
 for invariant failues; (2) introduce three kinds of evaluation context
 and show that steps inside typed code do not raise tag errors.
@citet{gdf-draft-2020} demonstrate the first method.
@citet{gf-icfp-2018} demonstrate the second.

@exact|{
\begin{definition}[TS$(\stypemap)$]
  Language\ $\slang$
  satisfies\ $\fTS{\stypemap}$
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

@exact|{
\begin{theorem}[type soundness]\leavevmode
  \begin{itemize}
    \item Language\ $\sU$ satisfies\ $\fTS{\stypemapzero}$
    \item Language\ $\sS$ satisfies\ $\fTS{\stypemapshape}$
    \item Language\ $\sT$ satisfies\ $\fTS{\stypemapone}$
  \end{itemize}
\end{theorem}
\begin{proof}
  \Lemmaref{lemma:model:completion} guarantees that the compiled form
   of the surface expression is well-typed.
  The rest follows from straightforward progress and preservation lemmas for the evaluation typing judgments.
  \Lemmaref{lemma:model:delta} is essential to preservation for primitive operations.
  Lemmas~\ref{lemma:model:su} and~\ref{lemma:model:boundary} are key aspects of preservation for boundary terms.
\end{proof}
}|

Complete monitoring is technically a statement about labeled expressions
 and a label-propagating reduction relation.
But, because the propagating reduction is derived from the basic reduction
 relation in a straightforward manner, our theorem statement uses the
 basic symbol (@${\srr}).
Likewise, both @${\sexpr_0} and @${\sexpr_1} refer to a labeled variant
 of an evaluation-language expression.
If no such labeling exist for a term, then the theorem holds vacuously.

@exact|{
\begin{theorem}[complete monitoring]
  If\ $~\sST \ssurface_0 : \stspec$
  and\ $\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and\ $\sowner_0; \cdot \Vdash \sexpr_0$
  and\ $\sexpr_0 \srr \sexpr_1$
  then\ $\sowner_0; \cdot \Vdash \sexpr_1$.
\end{theorem}
\begin{proof}
  By a preservation argument.
  The proofs for each basic reduction step are sketched below.
  These depend on two metafunctions: $\srev$ reverses a sequence of labels
   and $\slast$ extracts the last (outermost) element of such a sequence.

  \begin{description}
  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition, \(\sowner_1; \cdot \sWL \obars{\stagerror}{\sowner_1}\).

  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \subitem
    \begin{enumerate}
    \item
      $\sownerlist_0$ is either all \sdeep{} labels or a mix of \sshallow{} and \suntyped{}, by single-owner consistency of the redex.
    \item
      similarly, $\sowner_1$ must match $\sownerlist_0$
    \item
      $\svalue_0$ is a pair, because $\sdelta$ is defined on it.
    \item
      both components of $\svalue_0$ are well-labeled, again by single-owner consistency on the redex.
    \item
      by the definition of $\sdelta$.
    \end{enumerate}

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\stagerror}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$ and $\sdelta$; note that the binary operators are not elimination forms.

  \item[Case:]
    \(\obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \subitem
    \begin{enumerate}
    \item\label{step:model:cm:1}
      $\sownerlist_0$ is all \sdeep{} or a mix of \sshallow{} and \suntyped{}, by single-owner consistency of the redex.
    \item\label{step:model:cm:2}
      $\sowner_2; \cdot \sWL \svalue_0$, also by single-owner consistency of the redex.
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}$, by steps~\ref{step:model:cm:1} and~\ref{step:model:cm:2}.
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \svar_0$ for each occurrence of $\svar_0$ in $\sexpr_0$, by single-owner consistency of the redex.
    \item
      by a substitution lemma.
    \end{enumerate}

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the previous case.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the other substitution cases.

  \item[Case:]
    \(\obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2} \snr\)
    \\\qquad\(\obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)
  \subitem
    \begin{enumerate}
    \item
      $\sowner_0; \cdot \sWL \svalue_0$, by single-owner consistency of the redex.
    \item
      $\sowner_2; \cdot \sWL \svalue_1$, again by the redex.
    \item
      $\sownerlist_1$ is either all \sdeep{} or a mix of \sshallow{} and \suntyped{}, again by the redex.
    \item
      by the definition of $\sWL$.
    \end{enumerate}

  \item[Case:]
    \(\obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\snoop{}$ boundary connects either:
     two \sdeep{} components, two \sshallow{} components, two \suntyped{} components, or one \sshallow{} and one \suntyped{} component.

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\sscan{}$ boundary only links an \suntyped{} component to a \sshallow{} component.

  \item[Case:]
    \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\swraperror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ewrap{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ewrap{\tpair{\stype_0}{\stype_1}}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1} \snr\)
    \\\qquad\(\obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.
    Note that the rule moves the elements of the pair in the redex into a new pair in the contractum.

  \item[Case:]
  \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\svalue_0}{\sowner_1}\)
  \\ where $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$ 
  \item[]
    by the definition of $\sWL$.

  \end{description}
\end{proof}
}|


