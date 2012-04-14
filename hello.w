% hello.w
 
\documentclass[a4paper,twocolumn]{article}
\newif\ifshowcode
\showcodetrue

\usepackage{latexsym}
%\usepackage{html}

\usepackage{listings}

\usepackage{color}
%\usepackage{framed}
\usepackage{textcomp}
%\definecolor{listinggray}{gray}{0.9}
%\definecolor{shadecolor}{HTML}{211e1e}
\lstset{
	tabsize=2,
	language=C,
    keepspaces=true,
    upquote=true,
    aboveskip=0pt,
    belowskip=0pt,
    framesep=0pt,
    rulesep=0pt,
    columns=fixed,
    showstringspaces=true,
    extendedchars=true,
    breaklines=true,
    prebreak = \raisebox{0ex}[0ex][0ex]{\ensuremath{\hookleftarrow}},
    frame=none,
    framerule=0pt,
    showtabs=false,
    showspaces=false,
    showstringspaces=false,
    %basicstyle=\color[HTML]{dadada},
	%rulecolor=\color[HTML]{dadada},
	%backgroundcolor=\color[HTML]{211E1E},
    %identifierstyle=\color[HTML]{bec337},%\ttfamily,
    %keywordstyle=\color[HTML]{6f61ff},
    %commentstyle=\color[HTML]{ED5B15},
    %stringstyle=\color[HTML]{ad9361}
}
\definecolor{linkcolor}{rgb}{0, 0, 0.7}
\usepackage[backref,raiselinks,pdfhighlight=/O,pagebackref,hyperfigures,breaklinks,colorlinks,pdfstartview=FitBH,linkcolor={linkcolor},anchorcolor={linkcolor},citecolor={linkcolor},filecolor={linkcolor},menucolor={linkcolor},pagecolor={linkcolor},urlcolor={linkcolor}]{hyperref}
\NWuseHyperlinks
%\renewcommand{\NWtarget}[2]{\hypertarget{#1}{#2}}
%\renewcommand{\NWlink}[2]{\hyperlink{#1}{#2}} 
\renewcommand{\NWtxtDefBy}{defined by}
\renewcommand{\NWtxtRefIn}{referenced in}
\renewcommand{\NWtxtNoRef}{not referenced}
\renewcommand{\NWtxtIdentsUsed}{Fragment uses}
\renewcommand{\NWtxtIdentsNotUsed}{(never used)}
\renewcommand{\NWtxtIdentsDefed}{Fragment defines}

\setlength{\oddsidemargin}{0in}
\setlength{\evensidemargin}{0in}
\setlength{\topmargin}{0in}
\addtolength{\topmargin}{-\headheight}
\addtolength{\topmargin}{-\headsep}
\setlength{\textheight}{8.9in}
\setlength{\textwidth}{6.5in}
\setlength{\marginparwidth}{0.5in}

\title{Hello \texttt{nuweb} World}
\date{}
\author{Shane Celis
\\ {\sl shane.celis@@uvm.edu}}

\begin{document}
\maketitle
\section{Introduction}
This document is my first literate program.  It seemed appropriate to
write a hello world program.  

The file \verb|hello.c| contains our \verb|main| function.  
      
@o hello.c -cc @{/* @f 
DO NOT EDIT - generated from hello.w. 
*/
@<Include files@>

@<main@>@}

We begin with our includes.  In this case just the \verb|stdio.h|.

@d Include files @{#include <stdio.h>
@| printf @}

The heart of our program, the \verb|main| function.  I added a label
to reference the comment @xcomment@x because my indentation was not
coming out right.

@d main 
@{int main(int argn, char **argv) {
  @<print statement@>
  /* Why isn't this thing indented? @xcomment@x */
}
@| main @}

Finally, our print statement.
@d print statement
@{
printf("Hello nuweb World!\n");
@} 

Why not add another while we're at it.
@d print statement
@{printf("Goodbye nuweb World!\n");
@}

\section{Exercising All the Commands}
Here is @@@@ which produces the @@ symbol.

Here is @@v @v. 




\appendix
\section{Index of Filenames}
@f
\section{Index of Fragments}
@m
\section{Index of User Specified Identifiers}
@u
\end{document}
