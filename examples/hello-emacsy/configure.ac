#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([hello-emacsy], [1.0], [bug-report@address])
AC_CONFIG_AUX_DIR([support/build-aux])

# We're going to use GNUMake and we're going to like it.
AM_INIT_AUTOMAKE([-Wall foreign -Wno-portability])

AC_CONFIG_MACRO_DIR([support/m4])
# Checks for programs.
AC_PROG_CC_C_O
AC_PROG_LIBTOOL

AC_CHECK_PROG(NOWEB_CHECK,noweb,yes)
if test x"$NOWEB_CHECK" != x"yes" ; then
    AC_MSG_WARN([Must have noweb installed to alter literate source code.])
fi
AM_CONDITIONAL([NOWEB], [test x"$NOWEB_CHECK" = xyes])

AC_CHECK_PROG(LATEXMK_CHECK,latexmk,yes)
if test x"$LATEXMK_CHECK" != x"yes" ; then
    AC_MSG_WARN([Must have latexmk installed to produce PDFs.])
fi
AM_CONDITIONAL([LATEXMK], [test x"$LATEXMK_CHECK" = xyes])

# Checks for libraries.
PKG_CHECK_MODULES([GUILE], [guile-2.0])
#AX_CHECK_GLUT
# The autoconf macro does not prefer these frameworks over the
# standard X11 locations.

AC_SUBST(GL_LDFLAGS, ["-framework OpenGL"])
AC_SUBST(GLUT_LDFLAGS, ["-framework GLUT"])
AC_SUBST(GLU_LDFLAGS, [])


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdio.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([puts])

# 
AC_SUBST(EMACSY_CFLAGS, ['-I${top_srcdir}/../..'])
AC_SUBST(EMACSY_LIBS, ['${top_srcdir}/../../libemacsy.a'])
AC_SUBST(imagesdir, ['${top_srcdir}/support/images'])
AC_SUBST(automakedir, ['${top_srcdir}/support/automake'])

# Output files.
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([ Makefile
                  src/Makefile 
                  ])
AC_OUTPUT
