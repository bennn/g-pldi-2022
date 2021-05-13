#lang scribble/acmart @acmsmall @10pt @screen

@(require "main.rkt" "bib.rkt")
@(require scriblib/figure (only-in scribble/core make-style))

@para[#:style 'pretitle @elem[#:style (make-style "appendix" '(exact-chars))]{}]

@title[#:tag "sec:appendix"]{Appendix}

@section[#:tag "appendix:rules"]{xxx}

surface language types

@figure[
"fig:appendix:surface-types"
@elem{Surface Typing}

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
    \\
    \stypeenv \sST \sexpr_1 : \stype_0
  }{
    \stypeenv \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tfun{\stype_0}{\stype_1}}
    \\
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

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn
  }{
    \stypeenv \sST \emodule{\sulang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
  }{
    \stypeenv \sST \emodule{\stlang}{\sexpr_0} : \tdyn
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
    \stypeenv \sST \emodule{\stlang}{\sexpr_0} : \stype_0
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
    \stypeenv \sST \emodule{\stlang}{\sexpr_0} : \tfloor{\stype_0}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv \sST \emodule{\sslang}{\sexpr_0} : \tfloor{\stype_0}
  }
\end{mathpar}
}|]

@figure[
  "fig:appendix:surface-completion"
  @elem{Surface to Evaluation Completion}

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
    \\\\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
    \\\\
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


