#lang scribble/acmart

@(require "main.rkt" "bib.rkt" (only-in "pict.rkt" typed-codeblock fig:any-wrap-y fig:no-wrap-y fig:index-of-y))
@(require scriblib/figure (only-in scribble/core make-style))

@para[#:style 'pretitle @elem[#:style (make-style "appendix" '(exact-chars))]{}]

@title[#:tag "sec:appendix"]{Appendix}

@; TOPICS from rest of paper
@; - [X] {Surface typing {appendix:rules}}
@; - [X] {@|sDeep| typing judgment (@appendixref{appendix:rules})}
@; - [X] {@|sShallow| typing (@appendixref{appendix:rules})}
@; - [X] {Untyped typing judgment (@appendixref{appendix:rules})}
@; - [X] {Surface-to-evaluation compilation (@appendixref{appendix:rules})}
@; - [X] intiuition for labeling {appendix:guidelines}
@; - [X] lemmas for progress, preservation, and compilation (deferred to the @appendixref{appendix:lemmas}).
@; - [X] 3way macro issues @appendixref{appendix:macro}
@; - [X] 3-way boundary in TR examples and more details @appendix{appendix:boundary-api}
@; - [X] Refer to the @appendixref{appendix:expressiveness} for example programs.

@section[#:tag "appendix:rules"]{Surface Typing, Completion, Evaluation Typing, and Label Consistency Judgments}

@Figure-ref{fig:appendix:surface-types} presents the full typing judgment
for the surface language.
@Figure-ref{fig:appendix:surface-completion} presents the surface-to-evaluation
compilation rules.
Three other figures present the evaluation typing judgments:
@figure-ref{fig:appendix:deep-type} for @|sdeep| types,
@figure-ref{fig:appendix:shallow-type} for @|sshallow| types, and
@figure-ref{fig:appendix:untyped-type} for dynamic typing.
@Figure-ref{fig:appendix:ownership-consistency} presents the consistency judgment for
labeled expressions.

@figure[
"fig:appendix:surface-types"
@elem{Surface typing judgment}

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

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
    \\
    \stypeenv \sST \ssurface_1 : \tdyn
  }{
    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \stype_0
    \\
    \stypeenv \sST \ssurface_1 : \stype_1
  }{
    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tpair{\stype_0}{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tfloor{\stype_0}
    \\
    \stypeenv \sST \ssurface_1 : \tfloor{\stype_1}
  }{
    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tfloor{\tpair{\stype_0}{\stype_1}}
  }

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

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
  }{
    \stypeenv \sST \eunop{\ssurface_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\\\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \stype_1
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \tfloor{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
    \\
    \stypeenv \sST \ssurface_1 : \tdyn
  }{
    \stypeenv \sST \ebinop{\ssurface_0}{\ssurface_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\
    \stypeenv \sST \sexpr_1 : \stype_1
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1}
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \tfloor{\stype_2}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
    \\
    \stypeenv \sST \ssurface_1 : \tdyn
  }{
    \stypeenv \sST \eapp{\ssurface_0}{\ssurface_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfun{\stype_0}{\stype_1}
    \\\\
    \stypeenv \sST \sexpr_1 : \stype_0
  }{
    \stypeenv \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_0}{\stype_1}}
    \\\\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \eapp{\sexpr_0}{\sexpr_1} : \tfloor{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sST \sexpr_0 : \stype_1
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_1}
  }
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
  \sD & \stype_0 & \stype_0 \\
  \sD & \stype_0 & \tfloor{\stype_0} \\
  \sD & \stype_0 & \tdyn \\
  \sS & \tfloor{\stype_0} & \stype_0 \\
  \sS & \tfloor{\stype_0} & \tfloor{\stype_0} \\
  \sS & \tfloor{\stype_0} & \tdyn \\
  \sU & \tdyn & \stspec_1
\end{array}\) \end{tabular}
}
}|]

@figure[
  "fig:appendix:surface-completion"
  @elem{Surface-to-evaluation compilation}

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

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tdyn \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \stype_0 \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tfloor{\stype_0} \scompile \sint_0
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
    \\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

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
  \sD & \stype_0 & \stype_0 & \enoop{\sexpr_1} \\
  \sS & \tfloor{\stype_0} & \tfloor{\stype_0} & \enoop{\sexpr_1} \\
  \sU & \tdyn & \tdyn & \enoop{\sexpr_1} \\
  \sD & \stype_0 & \tdyn & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sS & \tfloor{\stype_0} & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sU & \tdyn & \stype_0 & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sD & \stype_0 & \tfloor{\stype_0} & \ewrap{\stype_0}{\sexpr_1} \!\! \\
  \sS & \tfloor{\stype_0} & \tdyn & \enoop{\sexpr_1} \!\! \\
  \sU & \tdyn & \tfloor{\stype_0} & \escan{\sshape_0}{\sexpr_1} \!\!
\end{array}\)
\\ & \qquad\hbox{where \(\sshape_0 = \fshape{\stype_0}\)}

\end{tabular}
}|]

@figure[
  "fig:appendix:deep-type"
  @elem{@|sDeep| typing judgment}

@exact|{
\lbl{\fbox{\(\stypeenv \sWTT \sexpr : \stype\)}}{
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
}}|]

@figure[
  "fig:appendix:shallow-type"
  @elem{@|sShallow| typing judgment}

@exact|{
\lbl{\fbox{\(\stypeenv \sWTS \sexpr : \sshape\)}}{
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
    \stypeenv \sWTS \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \kfun
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
    \\\\
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
}
}|]


@figure[
  "fig:appendix:untyped-type"
  @elem{Untyped typing judgment}
  @; ... aka dynamic typing, most types checked at runtime

@exact|{
\lbl{\fbox{\(\stypeenv \sWTU \sexpr : \tdyn\)}}{\begin{mathpar}
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
    \stypeenv \sWTU \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \tdyn
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
}}|]

@figure[
  "fig:model:ownership-consistency"
  @elem{@|sDeep| label consistency}

@exact|{
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
    \sowner_0; \sownerenv_0 \sWL \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \esfun{\svar_0}{\sshape_0}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\svar_0}{\sexpr_0}
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
}|]



@section[#:tag "appendix:guidelines"]{How to Lift a Reduction Relation}
@; 2021-06-12 TODO edit

@subsection{Background on Complete Monitoring}
@; --- begin CUT???
The idea behind complete monitoring is to test whether a semantics
has control over every interaction between typed and untyped code.
If the property holds, then a programmer can rely on the language to insert
 checks at the right places, for example, between the library and client
 demonstrated in @figureref{fig:ds-example}.
As a concrete example, if a value passes through the type @${(\tfun{\tint}{\tint})}
 then complete monitoring guarantees that the language has control over
 every input to the function and every result that the function computes,
 regardless of what context these interactions occur in.

Because all such interactions originate at the boundaries
 between typed and untyped code,
 a very simple way to formalize complete monitoring is to ask whether each
 boundary comes with a full run-time check when possible and an error otherwise.
A language that meets this strict requirement certainly has full control.
Other good designs fail, though.
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
Otherwise, the sender and client may have to assume joint ownership of the value
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

The key single-ownership rules (@figure-ref{fig:appendix:ownership-consistency})
deal with labeled expressions and boundary terms.
For example, every label in a deep-typed component must match a (possibly different)
deep-typed component, and the subterm in every @${\sscan} boundary must match
its server label:

@exact|{
\smallskip
\(\begin{array}[t]{ll@{~~}l}
\raisebox{6mm}{\fbox{$\sownerenv; \sowner \sWL \sexpr$}}
&
  \inferrule*{
    \sdowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sdowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\sdowner_1}
  }
&
  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \escan{\sshape_0}{\obars{\sexpr_0}{\sowner_1}}
  }
\end{array}
\)
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
@; --- end CUT???


@subsection{Lifting a Reduction Relation}

In practice, a language comes with an unlabeled reduction relation,
 and it is up to a researcher to design a lifted relation that handles labeled terms.
Lifting requires insight to correctly transfer labels
 and to ensure that labels do not change the behavior of programs.
If labels do not transfer correctly, then a complete monitoring theorem becomes
 meaningless.
And if the lifted relation depends on labels to compute a result, then
 a complete monitoring theorem says nothing about the original reduction relation.

The following informal guidelines
 explain how to lift a reduction relation.
They convey the intuitions behind our formulation of complete monitoring
and those of prior work@~cite{dfff-popl-2011,dtf-esop-2012,tsdtf-oopsla-2012,mdffc-oopsla-2016}.
Each guideline describes a way that labels may be transferred or dropped
 during evaluation.
To convey the general idea, each guideline also comes with a brief illustration, namely,
 an example reduction and a short comment.
The example reductions use a hypothetical @${\samplerrarrow} relation
 over the surface language.
Recall that @${\sstat} and @${\sdyn} are boundary terms; they link two
 components, a context and an enclosed expression, via a type.

When reading an example, accept the transitions
 @${\sexpr\!\samplerrarrow\!\sexpr} as axioms and focus on how the labels change
 in response:

@exact|{
{\begin{enumerate}
    \itemsep1ex
    \item \label{guideline:base}
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

    %[guideline of no-check transfer]
    \item \label{guideline:cross}
      Any other value that crosses a boundary must acquire the label of
      the new context.
    \subitem\hfill
      $\newcommand{\thevalue}{\epair{{-2}}{1}}
                \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\thevalue}{\sowner_1}}}{\sowner_0}
                \samplerrarrow \obbars{\thevalue}{\fconcat{\sowner_1}{\sowner_0}}$
    \subitem\hfill
      \emph{The pair\/ $\epair{{-2}}{1}$ does not match the type\/ $\tnat$.}

    \item \label{guideline:pos}
      Every value that flows out of a value $\svalue_0$
      acquires the labels of $\svalue_0$ and the context.
    \subitem\hfill
      $\obars{\ssnd~{\obbars{\epair{\obars{1}{\sowner_0}}{\obars{2}{\sowner_1}}}{\fconcat{\sowner_2}{\sowner_3}}}}{\sowner_4}
       \samplerrarrow \obbars{2}{\fconcat{\sowner_1}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\sowner_4}}}}$
    \subitem\hfill
      \emph{The value\/ $2$ flows out of the pair\/ $\epair{1}{2}$.}

    \item \label{guideline:neg}
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
      \emph{The substituted body flows out of the function,}
    \subitem\hfill
      \emph{and by \guidelineref{guideline:pos} acquires the function's labels.}

    \item \label{guideline:new}
      A primitive operation ($\sdelta$) may remove labels on incoming base values.
    \subitem\hfill
      $\obars{\ssum~{\obars{2}{\sowner_0}}~{\obars{3}{\sowner_1}}}{\sowner_2}
       \samplerrarrow \obars{5}{\sowner_2}$
    \subitem\hfill
      \emph{Assuming\/ $\sdelta(\ssum, 2, 3) = 5$.}

    \item \label{guideline:dup}
      Consecutive equal labels may be dropped.
    \subitem\hfill
      $\obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}} \eeq \obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}$

    \item \label{guideline:error}
      Labels on an error term may be dropped.
    \subitem\hfill
      $\obars{\edynb{\obnd{\sowner_0}{\tint}{\sowner_1}}{(\ssum~{9}~{\obars{\divisionbyzeroerror}{\sowner_1}})}}{\sowner_0}
       \samplerrarrow \divisionbyzeroerror$

  \end{enumerate}}
}|

@|noindent|Although @exact{\guidelineref{guideline:neg}} talks about functions, it generalizes to
 reference cells and other higher-order values.


@section[#:tag "appendix:lemmas"]{Lemmas for the Model}

@exact|{
\begin{theorem}[complete monitoring]
  %% If\ $~\sST \ssurface_0 : \stspec$
  If\ $~\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and\ $\sowner_0; \cdot \Vdash \sexpr_0$
  and\ $\sexpr_0 \srrlbl \sexpr_1$
  then\ $\sowner_0; \cdot \Vdash \sexpr_1$.
\end{theorem}
\begin{proofsketch}
  By a preservation argument.

  \begin{description}
  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obars{\stagerror}{\sowner_1}\)
  \item[]
    by definition \(\sowner_1; \cdot \sWL \obars{\stagerror}{\sowner_1}\)

  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}\)
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
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snrlbl \obars{\stagerror}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snrlbl \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$ and $\sdelta$ (binary operators are not elimination forms in the model, thus the number
    computed by $\sdelta$ does not acquire labels from $\svalue_0$ and $\svalue_1$)

  \item[Case:]
    \(\obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1} \snrlbl \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \!\snrlbl\! \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\!\)
    \begin{enumerate}
    \item\label{step:model:cm:1}
      $\sownerlist_0$ is all \sdeep{} or a mix of \sshallow{} and \suntyped{}, by \sdeep{}-label consistency of the redex
    \item\label{step:model:cm:2}
      $\sowner_1; \cdot \sWL \svalue_0$, also by \sdeep{}-label consistency of the redex
    \item
      let $\sowner_n$ be the rightmost label in the sequence $\sownerlist_0$
    \item
      $\sowner_n; \cdot \sWL \obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}$, by steps~\ref{step:model:cm:1} and~\ref{step:model:cm:2}
    \item
      $\sowner_n; \cdot \sWL \svar_0$ for each occurrence of $\svar_0$ in $\sexpr_0$, by \sdeep{}-label consistency of the redex
    \item
      by a substitution lemma
    \end{enumerate}

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrlblbreak \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the previous case

  \item[Case:]
    \(\obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrlbl \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\eappu{\obbars{\esfun{\svar_0}{\sshape_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snrlblbreak \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the other substitution cases

  \item[Case:]
    \(\obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2} \snrlblbreak
      \obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)
    \begin{enumerate}
    \item
      $\sowner_0; \cdot \sWL \svalue_0$, by \sdeep{}-label consistency of the redex
    \item
      $\sowner_2; \cdot \sWL \svalue_1$, again by \sdeep{}-label consistency
    \item
      $\sownerlist_1$ is either all \sdeep{} or a mix of \sshallow{} and \suntyped{}, again by the consistency of the redex
    \item
      by the definition of $\sWL$
    \end{enumerate}

  \item[Case:]
    \(\obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1} \snrlbl \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\snoop{}$ boundary connects either
     two \sdeep{} components or a mix of \sshallow{} and \suntyped{} components
     (self edges or ${\sS}$ to ${\sU}$)

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\sscan{}$ boundary links only \sshallow{} and/or \suntyped{} components

  \item[Case:]
    \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obars{\swraperror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ewrap{(\tfun{\stype_0}{\stype_1})}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$

  \item[Case:]
    \(\obars{\ewrap{(\tpair{\stype_0}{\stype_1})}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1} \snrlblbreak
      \obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$; the step makes a new pair

  \item[Case:]
  \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snrlbl \obars{\svalue_0}{\sowner_1}\)
  \\\hbox{}\qquad where $\stype_0 \in \tint \cup \tnat$ and $\fshapematch{\stype_0}{\svalue_0}$
  \item[]
    by the definition of $\sWL$

  \end{description}
\end{proofsketch}
}|

@exact|{
\begin{lemma}[compilation]\label{lemma:model:completion}
  If\ $~\sST \ssurface_0 : \stspec$
  then\ $~\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and either:
  \begin{itemize}
  \item $\stspec \in \stype$
        and\ $~\sWTT \sexpr_0 : \stspec$
  \item $\stspec \in \tfloor{\stype}$
        and\ $~\sWTS \sexpr_0 : \ftypemapshape{\stspec}$
  \item $\stspec \in \tdyn$
        and\ $~\sWTU \sexpr_0 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@exact|{
\begin{lemma}[type progress]
  If\ $~\sST \sexpr_0 : \stspec$
  then either\ $\sexpr_0 \in \svalue \cup \serror$
  or\ $\sexpr_0 \scc \sexpr_1$
\end{lemma}
}|

@exact|{
\begin{lemma}[type preservation]
  If\ $~\sST \sexpr_0 : \stspec$
  and\ $\sexpr_0 \scc \sexpr_1$
  then\ $~\sST \sexpr_1 : \stspec$
\end{lemma}
}|

@exact|{
\begin{lemma}[decomposition]
  For all\ $\sexpr_0$
  there exists unique\ $\sexpr_1, \sctx_0$
  such that\ $\sexpr_0 \sexpreq \finhole{\sctx_0}{\sexpr_1}$
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[$\sdelta, \sDelta$ agreement]\label{lemma:model:delta}\leavevmode
@;  \begin{itemize}
@;    \item
@;      If\ $~\sDelta(\sunop, \tdyn) = \tdyn$
@;      and\ $\sWTU \svalue_0 : \tdyn$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
@;      then\ $\sWTU \sdelta(\sunop, \svalue_0) : \tdyn$
@;    \item
@;      If\ $~\sDelta(\sunop, \sshape_0) = \sshape_1$
@;      and\ $\sWTS \svalue_0 : \sshape_0$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
@;      then\ $\sWTS \sdelta(\sunop, \svalue_0) : \sshape_1$
@;    \item
@;      If\ $~\sDelta(\sunop, \stype_0) = \stype_1$
@;      and\ $\sWTD \svalue_0 : \stype_0$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
@;      then\ $\sWTD \sdelta(\sunop, \svalue_0) : \stype_1$
@;    \item
@;      If\ $~\sDelta(\sbinop, \tdyn, \tdyn) = \tdyn$
@;      and\ $\sWTU \svalue_0 : \tdyn$
@;      and\ $\sWTU \svalue_1 : \tdyn$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
@;      then\ $\sWTU \sdelta(\sbinop, \svalue_0, \svalue_1) : \tdyn$
@;    \item
@;      If\ $~\sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2$
@;      and\ $\sWTS \svalue_0 : \sshape_0$
@;      and\ $\sWTS \svalue_1 : \sshape_1$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
@;      then\ $\sWTS \sdelta(\sbinop, \svalue_0, \svalue_1) : \sshape_2$
@;    \item
@;      If\ $~\sDelta(\sbinop, \stype_0, \stype_1) = \stype_2$
@;      and\ $\sWTD \svalue_0 : \stype_0$
@;      and\ $\sWTD \svalue_1 : \stype_1$
@;    \item[]
@;      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
@;      then\ $\sWTD \sdelta(\sbinop, \svalue_0, \svalue_1) : \stype_2$
@;  \end{itemize}
@;\end{lemma}
@;}|

@;@exact|{
@;\begin{lemma}[type substitution]\leavevmode
@;  \begin{itemize}
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      and\ $\fshapematch{\sshape_0}{\svalue_0}$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \kfun$
@;      and\ $~\vdash \svalue_0 : \sshape_0$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \esfun{\svar_0}{\sshape_0}{\sexpr_0} : \kfun$
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
  If\ $~\sWTS \sexpr_0 : \sshape_0$
  then\ $~\sWTU \sexpr_0 : \tdyn$
\end{lemma}
\begin{proof}
  By definition, in particular because the untyped rules
  allow shape-annotated functions.
\end{proof}
}|

@;@exact|{
@;\begin{lemma}[type in-hole]
@;  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
@;  then\ $\fexistsone{\stspec_1} \vdash \sexpr_0 : \stspec_1$
@;\end{lemma}
@;}|
@;
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
      If\ $~\sWTlang \svalue_0 : \stspec$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $~\sWTS \svalue_0 : \sshape_0$
    \item
      If\ $~\sWTS \svalue_0 : \sshape_0$
      then\ $~\sWTU \svalue_0 : \tdyn$
    \item
      If\ $~\sWTD \svalue_0 : \stype_0$
      and\ $\ewrap{\stype_0}{\svalue_0} \snr \svalue_1$
      then\ $~\sWTS \svalue_1 : \ftypemapshape{\stype_0}$
      and\ $~\sWTU \svalue_1 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@exact|{
\begin{lemma}[owner preservation]
  If\ $~\vdash \sexpr_0 : \stspec$
  and\ $\sowner_0 \Vdash \sexpr_0$
  and\ $\sexpr_0 \snr \sexpr_1$
  then\ $\sowner_0 \Vdash \sexpr_1$
\end{lemma}
}|

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



@;@subsection[#:tag "sec:implementation:impl:syntax"]{Syntax Re-Use}
@;
@;One limitation is that @|sShallow| code cannot use macros from @|sDeep| Racket modules.
@;Re-use is desirable to avoid copying code, but it requires a static analysis
@; to enforce soundness.
@;Consider the following simple macro.
@;It applies a typed function @tt{f} to an input, and is consequently
@; unsafe to send from @|sDeep| to @|sShallow| code:
@;
@;@typed-codeblock['("(define-syntax-rule (call-f x) (f x))")]
@;
@;@|noindent|If this macro could appear in a @|sShallow| Racket context, then any
@; @|sShallow| value @tt{x} could sneak into the @|sDeep| function.
@;Unless @tt{f} makes no assumptions about its input, such values can break
@; the @|sDeep| soundness guarantee and lead to dangerous results in optimized
@; code.
@;
@;One possible fix is to put a contract around every @|sDeep| identifier that
@; appears in a macro.
@;Doing so would require an analysis to find out which contracts are needed,
@; and a second analysis to install contracts wisely;
@; each identifier requires a contract, but repeated occurrences of one identifier
@; should not lead to repeated contract checks.
@;It may also be possible to avoid the contracts if the macro goes only to @|sDeep| clients.
@;
@;Another possibility is to statically check whether a macro is safe to
@; export.
@;Safe macros appear, for example, in the typed compatibility layer for the
@; RackUnit testing library.
@;RackUnit is an untyped library that exports some functions and some macros.
@;The typed layer provides types for the functions and type-annotated copies
@; of the macros (about 300 lines in total).
@;For example, the following macro combines a sequence of expressions into
@; a named RackUnit test case:
@;
@;@typed-codeblock['(
@;  "(define-syntax (test-case stx)"
@;  " (syntax-parse stx"
@;  "  [(_ name expr ...)"
@;  "   #'(parameterize ([test-name (ensure-str name)])"
@;  "       (test-begin expr ...))]))"
@;)]
@;
@;@|noindent|This macro is safe for @|sShallow| clients, but for complicated reasons.
@;First, @tt{ensure-str} is a typed function that accepts any input.
@;Second, @tt{test-begin} is a macro from the same file that is also safe.
@;Third, @tt{parameterize} comes from untyped Racket.
@;
@;@; ;; rackunit/rackunit-typed/rackunit/main.rkt
@;@;(define-syntax (test-begin stx)
@;@;  (syntax-case stx ()
@;@;    [(_ expr ...)
@;@;     (syntax/loc stx
@;@;       ((current-test-case-around)
@;@;        (lambda ()
@;@;          (with-handlers ([(λ (e)
@;@;                             (and (exn:fail? e)
@;@;                                  (not (exn:test? e))))
@;@;                           (λ ([e : exn:fail])
@;@;                             (test-log! #f)
@;@;                             (raise e))])
@;@;          (parameterize ([current-check-handler raise])
@;@;            (void)
@;@;            expr ...)))))]
@;@;    [_
@;@;     (raise-syntax-error
@;@;      #f
@;@;      "Correct form is (test-begin expr ...)"
@;@;      stx)]))
@;
@;Currently, the author of a @|sDeep| library can enable syntax re-use by
@;disabling the optimizer and unsafely providing macros.
@;This work-around requires a manual inspection, but it is more appealing than
@;forking the RackUnit library and asking programmers to import the version
@;that matches their use own of @|sDeep| and @|sShallow|.


@section[#:tag "sec:racket-users"]{Expressiveness by @|sShallow|: Example Programs}

@subsection{Less-strict Top Type}

@user-inspiration["sec:evaluation:expr:any" '(
 ("Denis Michiels"
  "error : Attempted to use a higher-order value passed as `Any` in untyped code"
  "2018-04-16"
  "https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ")
 ("Marc Kaufmann"
  "Typed Racket: 'Unable to protect opaque value passed as `Any`' with interesting behavior"
  "2019-12-11"
  "https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ"))]


@|sDeep| Racket enforces the top type with a wrapper
that prevents clients from inspecting the enclosed value.
This wrapper is a surprise for developers who expect programs such
 as @figure-ref{fig:evaluation:any-wrap} to run without error.
This program defines a mutable box in typed code,
 assigns the top type (@tt{Any}) to the box,
 and sends it to untyped code.
The untyped module attempts to set the box.
@|sDeep| Racket raises an exception when untyped code tries to modify the box.
Unfortunately for the programmer, this error is essential for soundness.
If untyped code put an integer in the box, then typed uses of the
 box would give a result that is inconsistent with its type.
@|sShallow| Racket runs the program without error because of its delayed
 checking strategy.
If @|sshallow|-typed code tries to read a symbol from the
 box, that access will raise an error.

Other top types for higher-order values have similar behavior.
For example, @|sShallow| Racket can import a function at the general @tt{Procedure} type,
cast to a more specific type, and apply the function.
A @|sDeep| cast only adds a second wrapper atop the
restrictive wrapper for the @tt{Procedure} type.
@; Indeed, because
@; @|sdeep| program can do so little with a @tt{Procedure} value, library authors
@; must use clever types to define generic utility functions for procedures.
@; Until recently, the @tt{object-name} function had a useless type
@; (@github-commit["racket" "typed-racket" "47a5ab3e2f335e6956aea4b98700d22a359ad6b2"]).


@subsection{No Missing Wrappers}

@figure[
  "fig:evaluation:no-wrap"
  @elem{@|sDeep| lacks wrappers for mutable pairs and a few other uncommon datatypes}
  fig:no-wrap-y]

@(let* ((missing-wrapper* '(
          "(Async-Channel T)" "(Custodian-Box T)" "(C-Mark-Key T)" "(Evt T)"
          "(Ephemeron T)" "(Future T)" "(MPair T T')" "(MList T)"
          "(Prompt-Tag T T')" "(Syntax T)" "(Thread-Cell T)" "(Weak-Box T)"))
        (num-missing (length missing-wrapper*)))
  @elem{
Several little-used types in @|sDeep| Racket
lack wrappers (@~a[num-missing] in total).
@Figure-ref{fig:evaluation:no-wrap} demonstrates the issue with a mutable pair
(@tt{MPairof}) type.
@|sDeep| Racket raises a run-time error when untyped code tries to call the @tt{add-mpair}
 function. @|sShallow| can run the program.
})

@subsection{Uniform Behavior}

@user-inspiration["sec:evaluation:expr:uniform" '(
 ("Bertrand"
  "Typed code from untyped code"
  "2020-02-17"
  "https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ")
 ("John B. Clements"
  "index-of + TR ... parametricity problem?"
  "2019-12-15"
  "https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ"))]

@figure[
  "fig:evaluation:index-of"
  @elem{@|sDeep| contracts can change the behavior of code}
  fig:index-of-y]

@Figure-ref{fig:evaluation:index-of} presents a typed module
that imports an untyped function, @tt{index-of}, with a precise
 polymorphic type.
The wrapper that enforces this type
 creates a new wrapper for every input to the function---to enforce parametric
 polymorphism@~cite{gmfk-dls-2007}.
Unfortunately, these input wrappers change the behavior of @tt{index-of};
 it searches the list for a wrapped version of the symbol @tt{'a} and returns
 a ``not found'' result (@tt{#false}) instead of the correct position (@tt{0}).
@|sShallow| Racket avoids all such changes in behavior
 because the @|stransient| semantics does not use wrappers to enforce types.


@exact|{
\begingroup
\setlength{\emergencystretch}{8em}
\printbibliography
\endgroup
}|
