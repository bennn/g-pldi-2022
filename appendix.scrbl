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

@section[#:tag "sec:laws"]{How to lift a reduction relation}
@; 2021-06-12 TODO edit

@; --- begin CUT???
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
@; --- end CUT???


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


@section[#:tag "sec:model:model:lemmas"]{Lemmas for Model}

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

@section[#:tag "sec:racket-users"]{User Inspiration}


@user-inspiration["sec:evaluation:expr:any" '(
 ("Denis Michiels"
  "error : Attempted to use a higher-order value passed as `Any` in untyped code"
  "2018-04-16"
  "https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ")
 ("Marc Kaufmann"
  "Typed Racket: 'Unable to protect opaque value passed as `Any`' with interesting behavior"
  "2019-12-11"
  "https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ"))]


@user-inspiration["sec:evaluation:expr:uniform" '(
 ("Bertrand"
  "Typed code from untyped code"
  "2020-02-17"
  "https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ")
 ("John B. Clements"
  "index-of + TR ... parametricity problem?"
  "2019-12-15"
  "https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ"))]



