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

@bold{Note}: The completion of a @|sshallow| function is very simple---to the point of
being misleading---because the evaluation language is too simple.
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
  \eappu{(\emon{\tfun{\stype_0}{\stype_1}}{\svalue_0})}{\svalue_1} & \snr
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
  \\\sidecond{if $\fshapematch{\fshape{\sshape_0}}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\tfun{\stype_0}{\stype_1}}{\svalue_0} & \snr
  & \emon{\tfun{\stype_0}{\stype_1}}{\svalue_0}
  \\\sidecond{if $\fshapematch{\kfun}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\tpair{\stype_0}{\stype_1}}{\epair{\svalue_0}{\svalue_1}} & \snr
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
  "fig:model:rrlbl"
  @elem{Labeled semantics for the evaluation language, derived from @figure-ref{fig:model:rr} and the guidelines in @section-ref{sec:laws}.}

@exact|{
\begin{rrarray}
  \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\stagerror}{\sowner_1}
  \\\sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sunop, \svalue_0)$ is undefined}
  \\[1.0ex]
  \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
  \\[1.0ex]
  \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
  & \snr
  & \obars{\stagerror}{\sowner_2}
  \\\sidecond{if $\svalue_i \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
  \\[1.0ex]
  \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
  & \snr
  & \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
  \\[1.0ex]
  \obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1}
  & \snr
  & \obars{\stagerror}{\sowner_1}
  \\\sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner} \cup \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obars{\sscanerror}{\sowner_1}
  \\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2}
  & \snr
  \\\sidecond{\qquad\(\obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)}
  \\[1.0ex]
  \obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1}
  & \snr
  & \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\sscanerror}{\sowner_1}
  \\\sidecond{if $\neg\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\fshapematch{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\swraperror}{\sowner_1}
  \\\sidecond{if $\fshapematch{\fshape{\sshape_0}}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  \\\sidecond{if $\fshapematch{\kfun}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\tpair{\stype_0}{\stype_1}}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1}
  & \snr
  \\\sidecond{\qquad\(\obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)}
  \\[1.0ex]
  \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\svalue_0}{\sowner_1}
  \\\sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$}
\end{rrarray}

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

The semantics of the evaluation syntax is based on one notion of reduction (@figure-ref{fig:model:rr}).
@; ... like how MT reuses host reduction, but don't get too excited we still play tricks with transient
Aside from the domain checks for @|sshallow|-typed functions, the reduction
rules are straightforward.
Unary and binary operations rely on the @${\sdelta} metafunction to compute a result (@figureref{fig:model:extra-rr}).
Basic function application substitutes an argument value into a function body.
Guard-wrapped function application decomposes into two wrap boundaries: one
 for the input and another for the result.
Lastly, boundary terms optionally perform a run-time check:
a @|snoop| boundary performs no check and lets any value cross;
a @|sscan| boundary checks the top-level shape of a value against the expected
type; and
a @|swrap| boundary checks top-level shapes and either installs a wrapper
around a higher-order value or recursively checks a data structure.

@Figure-ref{fig:model:extra-rr} defines evaluation metafunctions.
The @${\sdelta} function gives semantics to primitives.
The @${\sshapematch} function matches a type shape against the outer structure of a value.


@section[#:tag "sec:model:model:ownership"]{Single-Owner Consistency}

@|sDeep| types are characterized by complete monitoring (@section-ref{sec:background}).
To state a complete monitoring theorem, the model needs a labeled syntax,
 a single-owner consistency judgment, and a reduction relation that propagates
 labels.

The labeled syntax permits an ownership label around any expression (@figure-ref{fig:model:ownership-syntax}).
For example, the terms @${\obars{4}{\sowner_0}} and @${\obars{\eapp{\svar_0}{\svar_1}}{\sowner_1}}
 illustrate one labeled value and one labeled expression.
Most terms may have zero or more labels.
Boundary terms are an exception;
 a @${\swrap}, @${\sscan}, or @${\snoop} boundary must have at least one label
 around its subexpression.
The notation @${\obbars{\sexpr_0}{\sownerlist_0}} matches an expression with a sequence of labels
 (@${\sownerlist_0}).

An ownership label @${\sowner_0} carries two pieces of information.
First is a typing discipline: @${\sdowner} for @|sdeep|,
 @${\ssowner} for @|sshallow|, and @${\suowner} for @|suntyped|.
Second is a natural number index to distinguish different labels.
Initially, in a well-formed expression, these labels state the original owner
 and typing of a subterm.
As expressions reduce to values and flow across boundaries, labels accumulate
 to show which components are partly responsible for these values.

Ultimately, the goal of our complete monitoring proof effort is to show that
 only @|sdeep|-typed code is responsible for @|sdeep|-typed expressions.
Both @|sshallow| and @|suntyped| may recklessly share values.
The single-owner consistency judgment in @figure-ref{fig:model:ownership-consistency}
 formalizes the target invariant by stating when
 an expression is consistent for label @${\sowner_0} and label environment
 @${\sownerenv_0}.
A variable must be bound to @${\sowner_0} in the label environment.
Non-boundary terms must have consistent subterms.
Boundary terms and guard wrappers are ownership switch points;
 a boundary is consistent if its subterm is consistent with respect to the
 label inside the boundary.
Finally, the rules for explicitly-labeled expressions impose a discipline
 on labels.
A @|sdeep|-labeled expression may have other @|sdeep| labels, but nothing weaker.
@|sShallow| and @|suntyped|-labeled expressions, by contrast, can mix together.

Reduction of a labeled expression begins with the rules for the evaluation language
 (@figure-ref{fig:model:rr}) and propagates labels according to the laws
 stated in @sectionref{sec:laws}.
@Figure-ref{fig:model:rrlbl} presents the rules in full.
In short, labels always accumulate unless a simple value meets a boundary with
 a matching type shape.
Even @|snoop| boundaries add a label; this is why ownership consistency allows
 sequences of @|sdeep| labels.


@subsection[#:tag "sec:laws"]{How to lift a reduction relation}

Complete monitoring tests whether a mixed-typed semantics has control over
 every interaction between typed and untyped code.
If the property holds, then a programmer can rely on the language to run
 checks at the proper points, for example, between the library and client
 demonstrated in @figureref{fig:tr-example}.
Concretely, if a value passes through the type @${(\tfun{\tint}{\tint})}
 then complete monitoring guarantees that the language has control over
 every input to the function and every result that the function computes,
 regardless of whether these interactions occur in a typed or untyped context.

Because all such interactions originate at the boundaries
 between typed and untyped code,
 a simplistic way to formalize complete monitoring is to ask whether each
 boundary comes with a full run-time check when possible and an error otherwise.
A language that meets this strict requirement certainly has full control.
However, other good designs fail.
Suppose typed code expects a pair of integers and a semantics initially
 admits any pair at the boundary but eventually checks that the pair contains integers.
Despite the incomplete check at the boundary, this delayed-checking semantics eventually
 performs all necessary checks and should satisfy a complete monitoring theorem.
Higher-order values raise a similar question because a single run-time check
 cannot prove that a function value always behaves a certain way.
Nevertheless, a language that checks every call and return is in full control
 of the function's interactions.

Our definition of complete monitoring translates these ideas about
 interactions and control into statements about @emph{ownership labels}@~cite{dfff-popl-2011}.
At the start of an evaluation, no interactions have occurred yet and every
 expression has one owner: the enclosing component.
The reduction of a boundary term is the semantics of an interaction in which
 a value flows from one sender component to a client.
At this point, the sender loses full control over the value.
If the value fully matches the type expectations of the client, then the loss
 of control is no problem and the client gains full ownership.
Otherwise, the sender and client may have to assume joint ownership of the value,
 depending on the nature of the reduction relation.
If a semantics can create a value with multiple owners, then it admits that
 a component may lose full control over its interactions with other components.

Technically, an ownership label @${{}^{\sowner_0}} names one source-code component.
Expressions and values come with at least one ownership label;
 for example, @${\obars{42}{\sowner_0}} is an integer with one owner
 and @${\obars{\obars{\obars{42}{\sowner_0}}{\sowner_1}}{\sowner_2}} is an
 integer with three owners, written @${\obbars{42}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_2}}}} for short.
A complete monitoring theorem requires two ingredients that manage these labels.
First, a reduction relation @${\samplerred}
 must propagate ownership labels to reflect interactions and checks.
Second, a single-ownership judgment @${\sWL} must test whether every value in an
 expression has a unique owner.
To satisfy complete monitoring, reduction must preserve single-ownership.

The key single-ownership rules deal with labeled expressions and boundary terms:

@exact|{
\smallskip
\lbl{\fbox{$\sownerenv; \sowner \sWL \sexpr$}}{\begin{mathpar}
    \inferrule*{
      \sownerenv_0; \sowner_0 \sWL \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWL \obars{\sexpr_0}{\sowner_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_1 \sWL \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWL \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0}
    }

\end{mathpar}}
}|

@|noindent|Values such as @${\obbars{42}{\fconcat{\sowner_0}{\sowner_1}}}
 represent a communication that slipped past the run-time checking protocol,
 and therefore fail to satisfy single ownership.
The client owns the wrapper, and the sender retains ownership of the enclosed value.

@; @exact|{
@; \definitionsketch{\textrm{complete monitoring}}{
@;   For all\/ ${}\sWL \sexpr_0$,
@;   any reduction\/ $\sexpr_0 \samplerred \sexpr_1$
@;   implies\/ ${}\sWL \sexpr_1$.
@; }\smallskip
@; }|
@; 
@; The definition of complete monitoring is deceptively simple because it assumes
@;  a reduction relation that correctly propagates labels.

In practice, a language comes with an unlabeled reduction relation,
 and it is up to a researcher to design a lifted relation that handles labeled terms.
Lifting requires insight to correctly transfer labels
 and to ensure that labels do not change the behavior of programs.
If labels do not transfer correctly, then a complete monitoring theorem becomes
 meaningless.
And if the lifted relation depends on labels to compute a result, then
 a complete monitoring theorem says nothing about the original reduction relation.

These lifted reduction relations come about semi-automatically through the
 following informal guidelines, or natural (scientific) laws, for proper labeling.
Each law describes a way that labels may be transferred or dropped
 during evaluation.
To convey the general idea, each law also comes with a brief illustration, namely,
 an example reduction and a short comment.
The example reductions use a hypothetical @${\samplerrarrow} relation
 over the surface language.
Recall that @${\sstat} and @${\sdyn} are boundary terms; they link two
 components, a context and an enclosed expression, via a type.
When reading an example, accept the transitions
 @${\sexpr\!\samplerrarrow\!\sexpr} as axioms and focus on how the labels change
 in response.

@exact|{
{\begin{enumerate}
    %% NOTE when editing laws, remember there is an 8th in technical.tex for transient
    \itemsep1ex
    \item \label{law:base}
      If a base value reaches a boundary with a matching base type,
      then the value must drop its current labels as it crosses the boundary.
      %% NOTE before we said 'may drop' to avoid being too-restrictive,
      %%  but if 'may' is possible there's an argument that Natural is not
      %%  a complete monitor ... nor any semantics that lets base values cross.
      %% 'must' is less confusing and avoids this interpretation
    \subitem\hfill $\newcommand{\thevalue}{0}
              \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obbars{\thevalue}{\fconcat{\sowner_2}{\sowner_1}}}}{\sowner_0}
              \samplerrarrow \obars{\thevalue}{\sowner_0}$
    \subitem\hfill
      \emph{The value\/ $0$ fully matches the type\/ $\tnat$.}

    %[law of no-check transfer]
    \item \label{law:cross}
      Any other value that crosses a boundary must acquire the label of
      the new context.
    \subitem\hfill
      $\newcommand{\thevalue}{\epair{{-2}}{1}}
                \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\thevalue}{\sowner_1}}}{\sowner_0}
                \samplerrarrow \obbars{\thevalue}{\fconcat{\sowner_1}{\sowner_0}}$
    \subitem\hfill
      \emph{The pair\/ $\epair{{-2}}{1}$ does not match the type\/ $\tnat$.}

    \item \label{law:pos}
      Every value that flows out of a value $\svalue_0$
      acquires the labels of $\svalue_0$ and the context.
    \subitem\hfill
      $\obars{\ssnd~{\obbars{\epair{\obars{1}{\sowner_0}}{\obars{2}{\sowner_1}}}{\fconcat{\sowner_2}{\sowner_3}}}}{\sowner_4}
       \samplerrarrow \obbars{2}{\fconcat{\sowner_1}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\sowner_4}}}}$
    \subitem\hfill
      \emph{The value\/ $2$ flows out of the pair\/ $\epair{1}{2}$.}

    \item \label{law:neg}
      Every value that flows into a function $\svalue_0$ acquires the label
      of the context and the reversed labels of $\svalue_0$.
    \subitem\hfill
      $\newcommand{\thevalue}{\epair{8}{6}}
       \obars{\sapp~{\obbars{\efun{\svar_0}{\sfst~{\svar_0}}}{\fconcat{\sowner_0}{\sowner_1}}}~{\obars{\thevalue}{\sowner_2}}}{\sowner_3}
       \samplerrarrow$
    \subitem\hfill
       $\newcommand{\thevalue}{\epair{8}{6}}
        \obars{\obbars{\sfst~{\obbars{\thevalue}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\fconcat{\sowner_1}{\sowner_0}}}}}}{\fconcat{\sowner_0}{\sowner_1}}}{\sowner_3}$
    \subitem\hfill
      \emph{The argument value\/ $\epair{8}{6}$ is input to the function.} 
    \subitem\hfill
      \emph{The substituted body flows out of the function, and}
    \subitem\hfill
      \emph{by \lawref{law:pos} acquires the function's labels.}

    \item \label{law:new}
      A primitive operation ($\sdelta$) may remove labels on incoming base values.
    \subitem\hfill
      $\obars{\ssum~{\obars{2}{\sowner_0}}~{\obars{3}{\sowner_1}}}{\sowner_2}
       \samplerrarrow \obars{5}{\sowner_2}$
    \subitem\hfill
      \emph{Assuming\/ $\sdelta(\ssum, 2, 3) = 5$.}

    \item \label{law:dup}
      Consecutive equal labels may be dropped.
    \subitem\hfill
      $\obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}} \eeq \obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}$

    \item \label{law:error}
      Labels on an error term may be dropped.
    \subitem\hfill
      $\obars{\edynb{\obnd{\sowner_0}{\tint}{\sowner_1}}{(\ssum~{9}~{\obars{\divisionbyzeroerror}{\sowner_1}})}}{\sowner_0}
       \samplerrarrow \divisionbyzeroerror$

  \end{enumerate}}
}|

@|noindent|Note: @exact{\lawref{law:neg}} talks about functions, but generalizes to
 reference cells and other values that accept input.

Although the design of a lifted reduction relation is a challenge
 for every language,
 the laws in this section bring across the intuition behind prior
 formalizations of complete monitoring@~cite{dfff-popl-2011,dtf-esop-2012,tsdtf-oopsla-2012,mdffc-oopsla-2016}
 and may help guide future work.

@figure*[
  "fig:model:ownership-syntax"
  @elem{Ownership syntax}

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
    \mid \efun{\tann{\svar}{\sshape}}{\sexpr}
    \mid \emon{\stype}{\obars{\svalue}{\sowner}}
    \mid \obars{\svalue}{\sowner}
  \\
  \sctx & \slangeq &
    \ldots \mid \obars{\sctx}{\sowner}
  \\
  \sowner & \slangeq &
    \stowner_0 \mid \stowner_1 \mid \ldots \mid
  %\\ & &
    \ssowner_0 \mid \ssowner_1 \mid \ldots \mid
  %\\ & &
    \suowner_0 \mid \suowner_1 \mid \ldots
  \\
  \sownerlist & \slangeq &
    \mbox{sequence of ownership labels ($\sowner$)}
  \\
  \sownerenv & \slangeq &
    \cdot \mid \fcons{\tann{\svar}{\sowner}}{\sownerenv}
\end{langarray}
}|]

@figure*[
  "fig:model:ownership-consistency"
  @elem{Single-owner consistency}

@exact|{
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
    \sowner_0; \sownerenv_0 \sWL \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}
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
    \stowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \stowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\stowner_1}
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
}|]


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


@subsection[#:tag "sec:model:model:lemmas"]{Lemmas}

@exact|{
\begin{lemma}[completion]\label{lemma:model:completion}
  If\ $\sST \ssurface_0 : \stspec$
  then\ $\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and either:
  \begin{itemize}
  \item $\stspec \in \stype$
        and\ $\sWTT \sexpr_0 : \stspec$
  \item $\stspec \in \tfloor{\stype}$
        and\ $\sWTS \sexpr_0 : \ftypemapshape{\stspec}$
  \item $\stspec \in \tdyn$
        and\ $\sWTU \sexpr_0 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[decomposition]
@;  For all\ $\sexpr_0$
@;  there exists unique\ $\sexpr_1, \sctx_0$
@;  such that\ $\sexpr_0 \sexpreq \finhole{\sctx_0}[\sexpr_1]$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[type progress]
@;  If\ $~\sST \sexpr_0 : \stspec$
@;  then either\ $\sexpr_0 \in \svalue \cup \serror$
@;  or\ $\sexpr_0 \scc \sexpr_1$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[type preservation]
@;  If\ $~\sST \sexpr_0 : \stspec$
@;  and\ $\sexpr_0 \scc \sexpr_1$
@;  then\ $\sST \sexpr_1 : \stspec$
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}[$\sdelta, \sDelta$ agreement]\label{lemma:model:delta}\leavevmode
  \begin{itemize}
    \item
      If\ $~\sDelta(\sunop, \tdyn) = \tdyn$
      and\ $\sWTU \svalue_0 : \tdyn$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTU \sdelta(\sunop, \svalue_0) : \tdyn$
    \item
      If\ $~\sDelta(\sunop, \sshape_0) = \sshape_1$
      and\ $\sWTS \svalue_0 : \sshape_0$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTS \sdelta(\sunop, \svalue_0) : \sshape_1$
    \item
      If\ $~\sDelta(\sunop, \stype_0) = \stype_1$
      and\ $\sWTD \svalue_0 : \stype_0$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTD \sdelta(\sunop, \svalue_0) : \stype_1$
    \item
      If\ $~\sDelta(\sbinop, \tdyn, \tdyn) = \tdyn$
      and\ $\sWTU \svalue_0 : \tdyn$
      and\ $\sWTU \svalue_1 : \tdyn$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTU \sdelta(\sbinop, \svalue_0, \svalue_1) : \tdyn$
    \item
      If\ $~\sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2$
      and\ $\sWTS \svalue_0 : \sshape_0$
      and\ $\sWTS \svalue_1 : \sshape_1$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTS \sdelta(\sbinop, \svalue_0, \svalue_1) : \sshape_2$
    \item
      If\ $~\sDelta(\sbinop, \stype_0, \stype_1) = \stype_2$
      and\ $\sWTD \svalue_0 : \stype_0$
      and\ $\sWTD \svalue_1 : \stype_1$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTD \sdelta(\sbinop, \svalue_0, \svalue_1) : \stype_2$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[type substitution]\leavevmode
@;  \begin{itemize}
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      and\ $\fshapematch{\sshape_0}{\svalue_0}$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \kfun$
@;      and\ $~\vdash \svalue_0 : \sshape_0$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \kfun$
@;      and\ $~\vdash \svalue_0 : \sshape_1$
@;      and\ $\fshapematch{\sshape_0}{\svalue_0}$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \kany$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}$
@;      and\ $~\vdash \svalue_0 : \stype_0$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \stype_1$
@;  \end{itemize}
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}\label{lemma:model:su}
  If\ $\sWTS \sexpr_0 : \sshape_0$
  then\ $\sWTU \sexpr_0 : \tdyn$
\end{lemma}
\begin{proof}
  By definition.
  The key rules are for shape-annotated functions.
\end{proof}
}|

@;@exact|{
@;\begin{lemma}[type in-hole]
@;  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
@;  then\ $\fexistsone{\stspec_1} \vdash \sexpr_0 : \stspec_1$
@;\end{lemma}
@;}|

@;@exact|{
@;\begin{lemma}[type replace]
@;  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
@;  and\ $~\vdash \sexpr_0 : \stspec_1$
@;  and\ $~\vdash \sexpr_1 : \stspec_1$
@;  then\ $~\vdash \finhole{\sctx_0}{\sexpr_1} : \stspec_0$
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}[boundary-crossing]\label{lemma:model:boundary}\leavevmode
  \begin{itemize}
    \item
      If\ $\sWTlang \svalue_0 : \stspec$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $\sWTS \svalue_0 : \sshape_0$
    \item
      If\ $\sWTS \svalue_0 : \sshape_0$
      then\ $\sWTU \svalue_0 : \tdyn$
    \item
      If\ $\sWTD \svalue_0 : \stype_0$
      and\ $\ewrap{\stype_0}{\svalue_0} \snr \svalue_1$
      then\ $\sWTS \svalue_1 : \ftypemapshape{\stype_0}$
      and\ $\sWTU \svalue_1 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[owner preservation]
@;  If\ $~\vdash \sexpr_0 : \stspec$
@;  and\ $\sowner_0 \Vdash \sexpr_0$
@;  and\ $\sexpr_0 \snr \sexpr_1$
@;  then\ $\sowner_0 \Vdash \sexpr_1$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[label in-hole]
@;  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
@;  then\ $\fexistsone{\sowner_1} \sowner_1 \Vdash \sexpr_0$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[label replace]
@;  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
@;  and\ $\sowner_1 \Vdash \sexpr_0$
@;  and\ $\sowner_1 \Vdash \sexpr_1$
@;  then\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_1}$
@;\end{lemma}
@;}|

