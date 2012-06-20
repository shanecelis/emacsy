% hello.w
  
\documentclass{report}
\newif\ifshowcode
\showcodetrue

\usepackage{microtype}
\usepackage{multicol}
\usepackage{etoolbox}
\newtoggle{proposal}
\togglefalse{proposal}
\usepackage{verbatim}
\usepackage{latexsym}
\usepackage{amsmath}
\usepackage{graphicx}
%\usepackage{url}
%\usepackage{html} 

\usepackage{listings}
%% \usepackage[scaled]{beramono}
%% \usepackage[T1]{fontenc}
%% \usepackage[T1]{fontenc} 
%% \usepackage{dejavu}
%% \usepackage[scaled]{DejaVuSansMono}
%\usepackage{inconsolata}
%\usepackage{minted}
%\usemintedstyle{monokai}
%\definecolor{bg}{RGB}{39,40,34}

\usepackage{color}
%\usepackage{framed}
\usepackage{textcomp}
%\definecolor{listinggray}{gray}{0.9}
%\definecolor{shadecolor}{HTML}{211e1e}
\definecolor{darkgray}{rgb}{0.95,0.95,0.95}
\lstset{
%  basicstyle=\ttfamily,
  backgroundcolor=\color{darkgray}, 
	tabsize=2,
	language=lisp,
  escapechar=\@@,
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
  frame=tb,
  %framerule=0pt,
  showtabs=false,
  showspaces=false,
  showstringspaces=false,
  keywords={lambda, define, define-syntax, syntax-rules, set, while, if, begin, define-module, use-module, use-modules, let, let*, }
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

%\newminted{scheme}{bgcolor=bg,mathescape,texcl}

%\title{Emacsy: An Extensible Macro System}
\title{Emacsy: An Extensible, Embedable, Emacs-like Macro System}
%\title{Emacsey: An Embedable Macro System for Embedding }
\date{}
\author{Shane Celis
\\ {\sl shane.celis@@gmail.com}}

\input{commands}
\begin{document}
\maketitle
\chapter{Introduction}
%\section{Introduction}
% What's the logo?  How about a gnu inside some other creature?
% Think an O'Reily animal with a gnu inside it.

Emacsy is inspired by the Emacs text editor, but it is not an attempt
to create another text editor. This project "extracts" the kernel of
Emacs that makes it so extensible.  There's a joke that Emacs is a
great operating system---lacking only a decent editor. Emacsy is the
Emacs OS sans the text editor.  Although Emacsy shares no code with
Emacs, it does share a vision.  This project is aimed at Emacs users
and software developers.

\subsection{Vision}

Emacs has been extended to do much more than text editing.  It can get
your email, run a chat client, do video
editing\footnote{\url{http://1010.co.uk/gneve.html}}, and more.  For
some the prospect of chatting from within one's text editor sounds
weird.  Why would anyone want to do that?  Because Emacs gives them so
much control.  Frustrated by a particular piece of functionality?
Disable it.  Unhappy with some unintuitive key binding?  Change it.
Unimpressed by built-in functionality?  Rewrite it.  And you can do
all that while Emacs is running.  You don't have to exit and
recompile.

The purpose of Emacsy is to bring the Emacs way of doing things to
other applications natively. In my mind, I imagine Emacs consuming
applications from the outside, while Emacsy combines with applications
from the inside---thereby allowing an application to be Emacs-like
without requiring it to use Emacs as its frontend. I would like to hit
\verb|M-x| in other applications to run commands. I would like to see
authors introduce a new version: ``Version 3.0, now extendable with
Emacsy.'' I would like hear power users ask, ``Yes, but is it Emacsy?''

\subsection{Motivation}

This project was inspired by my frustration creating interactive
applications with the conventional edit-run-compile style of
development. Finding the right abstraction for the User Interface (UI)
that will compose well is not easy. Additionally, If the application
is a means to an end and not an end in itself (which is common for
academic and in-house tools), then the UI is usually the lowest
development priority.  Changing the UI is painful, so often mediocre
UIs rule.  Emacsy allows the developer---or the user---to reshape and
extend the UI and application easily at runtime.

\begin{figure}
  \centering
  \includegraphics[scale=0.4]{emacsy-logo.pdf} 
  \caption[Short Label]{\label{emacsy-logo}The proposed logo features
    a small gnu riding an elephant.}
\end{figure}


\subsection{Overlooked Treasure}

Emacs has a powerful means of programmatically extending itself while
it is running.  Not many successful applications can boast of that,
but I believe a powerful idea within Emacs has been overlooked as an
Emacsism rather than an idea of general utility.  Let me mention
another idea that might have become a Lispism but has since seen
widespread adoption.

The Lisp programming language introduced the term Read-Eval-Print-Loop
(REPL, pronounced rep-pel), an interactive programming feature present
in many dynamic languages: Python, Ruby, MATLAB, Mathematica, Lua to
name a few.  The pseudo code is given below.

@{
(while #t
 (print (eval (read))))
@}

The REPL interaction pattern is to enter one complete expression, hit
the return key, and the result of that expression will be displayed.
It might look like this:

\begin{verbatim}
> (+ 1 2)
3
\end{verbatim}\label{blah}

The kernel of Emacs is conceptually similar to the REPL, but the level
of interaction is more fine grained.  A REPL assumes a command line
interface.  Emacs assumes a keyboard interface.  I have not seen the
kernel of Emacs exhibited in any other applications, but I think it is
of similar utility to the REPL---and entirely separate from text
editing.  I'd like to name this the Key-Lookup-Execute-Command-Loop
(KLECL, pronounced clec-cull).

@{
(while #t
 (execute-command (lookup-key (read-key))))
@}

Long-time Emacs users will be familiar with this idea, but new Emacs
users may not be.  For instance, when a user hits the 'a' key, then an
'a' is inserted into their document.  Let's pull apart the functions
to see what that actually looks like with respect to the KLECL.


\begin{verbatim}
> (read-key)
#\a
> (lookup-key #\a)
self-insert-command
> (execute-command 'self-insert-command)
#t
\end{verbatim}

Key sequences in Emacs are associated with commands.  The fact that
each command is implemented in Lisp is an implementation detail and
not essential to the idea of a KLECL.

Note how flexible the KLECL is: One can build a REPL out of a KLECL,
or a text editor, or a robot simulator (as shown in the video).  Emacs
uses the KLECL to create an extensible text editor.  Emacsy uses the
KLECL to make other applications similarly extensible.

\subsection{Goals}

The goals of this project are as follows.

\begin{enumerate}
\item Easy to embed technically

  Emacsy will use Guile Scheme to make it easy to embed within C and C++ programs.  

\item Easy to embed legally

  Emacsy will be licensed under the LGPL.  

\item Easy to learn

  Emacsy should be easy enough to learn that the uninitiated may easily
  make parametric changes, e.g., key 'a' now does what key 'b' does
  and \emph{vice versa}.  Programmers in any language ought to be able
  to make new commands for themselves.  And old Emacs hands should be
  able to happily rely on old idioms and function names to change most
  anything.

\item Opinionated but not unpersuadable

  Emacsy should be configured with a sensible set of defaults
  (opinions).  Out of the box, it is not \emph{tabla rasa}, a blank
  slate, where the user must choose every detail, every time.
  However, if the user wants to choose every detail, they can.

\item Key bindings can be modified

  It wouldn't be Emacs-like if you couldn't tinker with it.

\item Commands can be defined in Emacsy's language or the host
  language
  
  New commands can be defined in Guile Scheme or C/C++.

\item Commands compose well
  
  That is to say, commands can call other commands.  No special
  arrangements must be considered in the general case.

\item A small number of \emph{interface} functions

  The core functions that must be called by the embedding application
  will be few and straightforward to use.

\item Bring KLECL to light

\end{enumerate}

\subsection{Anti-goals}

Just as important as a project's goals are its anti-goals: the things
it is not intended to do.

\begin{enumerate}
\item Not a general purpose text editor

  Emacsy will not do general purpose text editing out of the box,
  although it will have a minibuffer.

\item Not an Emacs replacement

  Emacs is full featured programmer's text editor with more bells and
  whistles than most people will ever have the time to fully explore.
  Emacsy extracts the Emacs spirit of application and UI extensibility
  to use within other programs.  

\item Not an Elisp replacement

  There have been many attempts to replace Emacs and elisp with an
  newer Lisp dialect.  Emacsy is not one of them. 

\item Not source code compatible with Emacs

  Although Emacsy may adopt some of naming conventions of Emacs, it
  will not use elisp and will not attempt to be in any way source code
  compatible with Emacs.

\item Not a framework
  
  I will not steal your runloop.  You call Emacsy when it suits your
  application not the other way around.

\end{enumerate}

\section{Emacsy Features}

These are the core features from Emacs that will be implemented in Emacsy.

\begin{enumerate}
\item keymaps
\item minibuffer
\item recordable macros
\item history
\item tab completion
\item major and minor modes
\end{enumerate}


\chapter{Usage}

%% \section{Implementation Plan}

%% One could go about implementing a project with the aforementioned
%% goals in many different ways.  Althought, I am interested in
%% separating the KLECL from Emacs in particular, for instance I think
%% implementing a KLECL in \verb|Lua| would be valuable, but I also want
%% more than just the KLECL.  I want to provide an Emacs-like way of
%% developing applications for developers of all stripes.

%% \subsection{Implementation Decisions}

%% \begin{enumerate}
%% \item Use GNU Guile Scheme.

%% \item Adopt Emacs naming conventions.

%% \item Write as a literate program.

%%   To assist in the goal of making this project understandable enough
%%   to trust, I plan to use
%%   \verb|nuweb|\footnote{\url{http://nuweb.sourceforge.net/}} and write
%%   this in literate programming fashion where the source code and
%%   documentation come from the same document.  

%% %\item Make code available on github
%% \end{enumerate}

\subsection{Minimal Emacsy Example}

\chapter{Implementation}

Emacsy is divided into the following modules: klecl, buffer.

\section{C API}

The minimal C API is given below.

\lstset{language=C}
@o emacsy.h -cc @{@%
/* @f

@< Copyright @>

@< License @>
*/

@< Begin Header Guard @>

@< Defines @>

/* Initialize Emacsy. */
int  emacsy_init(void);

/* Enqueue a keyboard event. */
void emacsy_key_event(int char_code,
                      int modifier_key_flags);

/* Enqueue a mouse event. */
void emacsy_mouse_event(int x, int y, 
                        int state,
                        int button,
                        int modifier_key_flags);

/* Run an iteration of Emacsy's event loop 
   (will not block). */
void emacsy_tick(); 

/* Return the message or echo area. */
char *emacsy_message_or_echo_area();

/* Return the mode line */
char *emacsy_mode_line();

@< End Header Guard @>
@%

@|@}

Here are the constants for the C API.

@d Defines @{@%
#define MODKEY_COUNT   6

#define MODKEY_ALT     1 // A
#define MODKEY_CONTROL 2 // C
#define MODKEY_HYPER   4 // H
#define MODKEY_META    8 // M
#define MODKEY_SUPER  16 // s
#define MODKEY_SHIFT  32 // S

#define MOUSE_BUTTON_DOWN  0
#define MOUSE_BUTTON_UP    1
#define MOUSE_MOTION       2
@|@}

@d Begin Header Guard @{@%
#ifdef __cplusplus
 extern "C" {
#endif
@|@}

@d End Header Guard @{@%
#ifdef __cplusplus
 }
#endif
@|@}



The implementation of the API basically calls similarly named Scheme
functions.

@o emacsy.c @{@%
#include "emacsy.h"
#include <libguile.h>

@< Utility Functions @>

int emacsy_init()
{
  scm_c_use_module("emacsy");
  /* load the emacsy modules */
  return 0;
}

void emacsy_key_event(int char_code,
                      int modifier_key_flags)
                      
{
  // XXX I shouldn't have to do a CONTROL key fix here.
  (void) scm_call_2(scm_c_public_ref("emacsy","emacsy-key-event"),
                    scm_integer_to_char(scm_from_char(char_code + (modifier_key_flags & MODKEY_CONTROL ? ('a' - 1) : 0))),
                    modifier_key_flags_to_list(modifier_key_flags));
}

void emacsy_mouse_event(int x, int y, 
                        int state,
                        int button, 
                        int modifier_key_flags)
{

  SCM down_sym   = scm_c_string_to_symbol("down");
  SCM up_sym     = scm_c_string_to_symbol("up");
  SCM motion_sym = scm_c_string_to_symbol("motion");
  SCM state_sym;
  switch(state) {
  case MOUSE_BUTTON_UP:   state_sym = up_sym;     break;
  case MOUSE_BUTTON_DOWN: state_sym = down_sym;   break;
  case MOUSE_MOTION:      state_sym = motion_sym; break;
  default:
    fprintf(stderr, "warning: mouse event state received invalid input %d.\n",
            state);
    return;
  }

  (void) scm_call_3(scm_c_public_ref("emacsy","emacsy-mouse-event"),
                    scm_vector(scm_list_2(scm_from_int(x),
                                          scm_from_int(y))),
                    scm_from_int(button + 1),
                    state_sym);
}

void emacsy_tick()
{
  (void) scm_call_0(scm_c_public_ref("emacsy",
                                      "emacsy-tick"));
}

char *emacsy_message_or_echo_area()
{
  return scm_to_locale_string(
    scm_call_0(scm_c_public_ref("emacsy",
                                 "emacsy-message-or-echo-area")));
}

char *emacsy_mode_line()
{
  return scm_to_locale_string(
    scm_call_0(scm_c_public_ref("emacsy",
                                 "emacsy-mode-line")));
}
@|@}

@d Utility Functions @{@%
SCM scm_c_string_to_symbol(const char* str) {
  return scm_string_to_symbol(scm_from_locale_string(str));
}

SCM modifier_key_flags_to_list(int modifier_key_flags)
{
  const char* modifiers[] = { "alt", "control", "hyper", "meta", "super", "shift" };
  SCM list = SCM_EOL;
  for (int i = 0; i < 6; i++) {
    if (modifier_key_flags & 1 << i) {
      list = scm_cons(scm_c_string_to_symbol(modifiers[i]), list);
    }
  }
  
  return list;
}
@|@}




\lstset{language=lisp}
\section{KLECL}

@o emacsy/klecl.scm -cl -d  @{@%
@< Lisp File Header @> 
(define-module (emacsy klecl)
  @< Include Modules @>
  #:export ( @< Exported Symbols @> )
  #:export-syntax ( @< Exported Syntax @> ) )
@< Variables @>
@< Procedures @>
@|@}


We will use the module check for most of our unit test needs.

@o emacsy-tests.scm -cl -d  @{@< Lisp File Header @>  
@< Test Preamble @>
(eval-when (compile load eval)
           (module-use! (current-module) (resolve-module '(emacsy)))) 
@< Tests @> 
@< Test Postscript @> 
@|@}

The header for Lisp files shown below.

@d Lisp File Header @{;;; @f 
;; DO NOT EDIT - generated from emacsy.w. 
;;
;; @< License @>
;;
;; @f written by Shane Celis 
;; shane (dot) celis (at) uvm (dot) edu
@|@}

@d Definitions @{@%
(define (f x)
  (1+ x))
@|f@}

@d Tests @{@%
(define-test (test-f)
  (check (f 1) => 2)
  (check (f 0) => 1)
)
@|test-f@}

\subsection{Literate Programming Support}

All the code for this project is generated from \verb|emacsy.w|.  To
ease debugging, it is helpful to have the debug information point to
the place it came from in \verb|emacsy.w| and not whatever source file
it came from.  The program \verb|nuweb| has a means of providing this
information that works for C/C++, the line pragma.  Scheme does not
support the line pragma, but the reader can fortunately be extended to
support it.

An example use of it might look like this:

@{@%
(define (f x)
#line 314 "increment-literately.w"
  (+ x 1)) @}

@d Line Pragma Handler @{@%
(lambda (char port)
  (let* ((ine (read port))
         (lineno (read port))
         (filename (read port)))
    (if (not (eq? ine 'ine))
        (error (format #f "Expected '#line <line-number> <filename>'; got '#~a~a ~a \"~a\"'." char ine lineno filename)))
    (set-port-filename! port filename)
    (set-port-line! port lineno)
    (set-port-column! port 0)
    ""))
@|@}

One problem that popped up was I sometimes wanted to include pieces of
documentation in embedded strings.  Something that might end up
looking like this in the source code:

@{
(define (f x)
  "#line 352 "emacsy.w"
   The function f and its associated function h...
   #line 362 "emacsy.w"
  "
  ...
@}

The above code will see a string "\#line 352 " followed by a bare
symbol emacsy.w, which will not do.  To get around this, I implemented
another reader extension that will strip out any \#l lines within it.

@d Liberal String Quote Reader @{@%
(lambda (char port)
  (let ((accum '()))
    (let loop ((entry (read-char port)))
      (if (or (eof-object? entry)
              (and (char=? #\" entry)
                   (char=? #\# (peek-char port))
                   (begin (read-char port)
                          #t)))
          ;; We're done
          (apply string (reverse accum))
          (begin
            (if (and (char=? #\# entry)
                     (char=? #\l (peek-char port)))
                ;; Drop this line
                (begin (read-line port)
                       (loop (read-char port)))
                (begin
                  ;; Keep and loop
                  (set! accum (cons entry accum))
                  (loop (read-char port)))))))))
@|@}

@o my-line-pragma.scm -cl @{@%
(define-module (my-line-pragma)
  #:use-module (ice-9 rdelim))

(eval-when (compile load eval)
 (read-hash-extend #\l @< Line Pragma Handler @>)
 (read-hash-extend #\" @< Liberal String Quote Reader  @>))

@|@}



\subsection{Unit Testing}

We want to be able to easily write and aggregate unit tests.  It's not
important to our project per se.  We just need the utility.  Our
association list (alist) \verb|unit-tests| will hold the symbol of the
function and the procedure.

@o check/harness.scm -cl -d @{@%
(define-module (check harness)
  #:use-module (check)
  #:export (run-tests
            run-tests-and-exit)
  #:export-syntax (define-test))
@|@}

Set up the variables.

@o check/harness.scm @{@%
(define unit-tests '())
(define test-errors '())
@|@}

We can register any procedure to a test name.  

@o check/harness.scm @{@%
(define (register-test name func)
  "Register a procedure to a test name."
  (set! unit-tests (acons name func unit-tests)))
@|@}

Typically, users will define and register their tests with this macro.

@o check/harness.scm @{@%
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
@|define-test@}

We need to run the tests.

@o check/harness.scm @{@%
(define (run-tests)
  (catch 'first-error
    (lambda () 
      @< handle each test @>)
    (lambda args
      #f)))
@|@}

@d handle each test @{@%
(for-each 
 (lambda (elt)
   (format #t "TEST: ~a\n" (car elt))
   ;;(pretty-print elt)
   (catch #t
     (lambda ()
       (with-throw-handler 
        #t
        (lambda ()
          (apply (cdr elt) '()))
        (lambda args
          (set! test-errors (cons (car elt) test-errors))
          (format #t "Error in test ~a: ~a" (car elt) args)
          (backtrace))))
     (lambda args
       (throw 'first-error)
       #f)))
 (reverse unit-tests))
@|@}

@o check/harness.scm @{@%
(define (run-tests-and-exit)
  (run-tests)
  (check-report)
  (if (> (length test-errors) 0)
      (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
      (format #t "NO ERRORs in tests."))
  (exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1)))
@|@}






@d test functions @{@%
(define unit-tests '())

(define (register-test name func)
  (set! unit-tests (acons name func unit-tests)))

@| unit-tests register-test@}

The function register-test does the work, but we don't want to require
the user to call it, so we'll define a macro that will automatically
call it.

@d test macro @{@%
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
@| define-test@}

Finally, now we just need a way to run all the unit tests.

@d run tests @{@%
(define test-errors '())
(define (run-tests)
  (catch 'first-error
    (lambda () (for-each (lambda (elt)
                           (display "TEST: ")
                           (pretty-print elt)
                 (catch #t
                   (lambda ()
                     (with-throw-handler #t
                                         (lambda ()
                                           (apply (cdr elt) '()))
                                         (lambda args
                                           (set! test-errors (cons (car elt) test-errors))
                                           (format #t "Error in test ~a: ~a" (car elt) args)

                                           (backtrace))))
                   (lambda args
                     ;(throw 'first-error)
                     #f
                     )))
               (reverse unit-tests)))
    (lambda args
      #f)))
@| run-tests test-errors@}

Finally, let's provide this as our testing preamble.

@d Test Preamble @{@%
(use-modules (check))
(use-modules (ice-9 pretty-print))

@< test functions @>
@< test macro @>
@< run tests @>
@|@}

Let's run these tests at the end.

@d Test Postscript @{@%

(run-tests)
(check-report)
(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))

@|@}

\section{Example Program: ERACS}

Extensible Robot And Controller Simulation (ERACS) was written along
side Emacsy.  

\section{Vector Math}

@o vector-math.scm -cl -d  @{@< Lisp File Header @> 
;@< Vector Module @> 
@< Vector Definitions @>
@|@}   


\begin{enumerate}
\item @< vector-component-usage @>

 The component of $\bv a$ in the $\bv b$ direction.
\begin{align*}
  \comp_\bv b \bv a &= \bv a \cdot \bhv b \\
  &= \frac{\bv a \cdot \bv b}{||\bv b||}
\end{align*}

  @d Vector Definitions @{(define (vector-component a b)
    ;(string-trim-both 
    #" @< vector-component-usage @> "# 
    ;char-set:whitespace)
 (/ (vector-dot a b) (vector-norm b)))
@|@}

@d vector-component-usage @[Scalar projection@]

\item Vector projection

  The vector projection of $\bv a$ on $\bv b$.
  \begin{align*}
    \proj_\bv b \bv a &= a_1 \bhv b \\
    a_1 &= \comp_\bv b \bv a
  \end{align*}
  @d Vector Definitions @{(define (vector-projection a b)
 (vector* (vector-component a b) (vector-normalize b)))
@|@}

\end{enumerate}







\appendix
\section{Index of Filenames}
@f
\section{Index of Fragments}
@m
\section{Index of User Specified Identifiers}
@u

\end{document}

%%  LocalWords:  elt args Emacsism lispism Eval rep pel MATLAB Lua
%%  LocalWords:  Mathematica eval int printf clec unpersuadable tabla
%%  LocalWords:  rasa Emacsy's
