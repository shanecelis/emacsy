
# ax_check_noweb entry point
AC_DEFUN([AX_CHECK_NOWEB],
[
AC_REQUIRE([AX_CHECK_OPEN])
AC_CHECK_PROG(NOWEB_CHECK,noweb,yes)
if test x"$NOWEB_CHECK" != x"yes" ; then
    AC_MSG_WARN([Must have noweb installed to alter literate source code.])
fi
AC_ARG_ENABLE([line_pragma],
              [AC_HELP_STRING([--enable-line-pragma=@<:@yes/no@:>@],
                              [Enable line pragma in noweb @<:@default=no@:>@])],
              [],
              [enable_line_pragma=no])

AM_CONDITIONAL([LINE_PRAGMA], [test x"$enable_line_pragma" = xyes])

AC_ARG_ENABLE([noweb],
              [AC_HELP_STRING([--enable-noweb=@<:@yes/no@:>@],
                              [Enable noweb @<:@default=yes@:>@])],
              [],
              [enable_noweb=yes])

AS_IF([test "x$enable_noweb" = "xyes"],
[
AC_PATH_PROG([noweb], noweb)
AC_SUBST(noweb)

AC_PATH_PROG([noweave], noweave)
AC_SUBST(noweave)

AC_PATH_PROG([notangle], notangle)
AC_SUBST(notangle)

AC_PATH_PROG([noroots], noroots)
AC_SUBST(noroots)

AC_PATH_PROG([GREP], [grep])
AC_SUBST([GREP])

noweb_home=`AS_DIRNAME(["$noweb"])`/..

AC_PATH_PROG([markup], markup, no, [$PATH:$noweb_home/libexec/noweb:$noweb_home/lib/noweb])
if test "$markup" = "no"; then
  AC_MSG_ERROR([Must have markup installed to alter literate source code.])
fi
AC_SUBST(markup)
],
[])


AM_CONDITIONAL([NOWEB], [test x"$NOWEB_CHECK" = xyes && test x"$enable_noweb" = xyes])

AC_ARG_ENABLE([pdflatex],
              [AC_HELP_STRING([--enable-pdflatex=@<:@yes/no@:>@],
                              [Enable pdflatex @<:@default=yes@:>@])],
              [enable_pdflatex=no],
              [enable_pdflatex=yes])
AC_CHECK_PROG(PDFLATEX_CHECK,pdflatex,yes)
if test x"$PDFLATEX_CHECK" != x"yes" ; then
    AC_MSG_WARN([Must have pdflatex installed to produce PDFs.])
fi
AM_CONDITIONAL([PDFLATEX], [test x"$PDFLATEX_CHECK" = xyes && test x"$enable_pdflatex" = xyes])
AC_PATH_PROG([pdflatex], pdflatex)
AC_SUBST(pdflatex)
])


