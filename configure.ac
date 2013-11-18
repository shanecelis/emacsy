#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
# We'll try to use semantic versioning.  http://semver.org
AC_INIT([emacsy], [0.1.2], [shane.celis@gmail.com])
AC_CONFIG_AUX_DIR([support/build-aux])
# Lies.
#AM_INIT_GUILE_MODULE

# We're going to use GNUMake and we're going to like it.
AM_INIT_AUTOMAKE([-Wall foreign -Wno-portability parallel-tests])
AM_EXTRA_RECURSIVE_TARGETS([run show-doc])
AM_SILENT_RULES([yes])
AC_CONFIG_MACRO_DIR([support/m4])
# Checks for programs.
AC_PROG_CC_C99
AC_PROG_CC_C_O
AC_PROG_LIBTOOL
AX_CHECK_NOWEB
AX_CHECK_OPEN
GUILE_PROGS
AC_PATH_PROG([PERL], [perl])
AC_SUBST([PERL])

AC_PATH_PROG([BASH], [bash])
AC_SUBST([BASH])

AC_PATH_PROG([guile_snarf], guile-snarf)
AC_SUBST(guile_snarf)

# Checks for libraries.
PKG_CHECK_MODULES([GUILE], [guile-2.0])

AM_CONDITIONAL([HELLO_EMACSY], [test -f example/hello-emacsy/README.md])

AC_SUBST(DISTDIR, ["${PACKAGE_TARNAME}-${PACKAGE_VERSION}"])

#PKG_CHECK_MODULES([EMACSY], [libemacsy])
#AX_CHECK_GLUT
# The autoconf macro does not prefer these frameworks over the
# standard X11 locations.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdio.h])

dnl AC_CHECK_HEADERS([tap/basic.h], [], [])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
dnl AC_CHECK_FUNCS([puts])

# 
dnl AC_SUBST(EMACSY_CFLAGS, ['-I${top_srcdir}/../..'])
dnl AC_SUBST(EMACSY_LIBS, ['${top_srcdir}/../../libemacsy.a'])
AC_SUBST(latexdir, ['${top_srcdir}/support/latex'])
AC_SUBST(imagesdir, ['${top_srcdir}/support/images'])
AC_SUBST(automakedir, ['${top_srcdir}/support/automake'])
AC_SUBST(helloemacsydir, ['${top_srcdir}/example/hello-emacsy/'])
pdflatex="TEXINPUTS=${latexdir}:${imagesdir}:${helloemacsydir}/support/images: $pdflatex";

AC_SUBST(ENV, ['${top_builddir}/bin/env'])

# Output files.
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([ Makefile
                  bin/Makefile
                  src/Makefile 
                  src/emacsy/Makefile 
                  test/Makefile 
                  test/works-without-noweb.sh
                  test/print-install.sh
                  support/pkg-config/emacsy.pc
                  support/pkg-config/not-installed/emacsy.pc
                  example/Makefile
                  ], [])

AC_CONFIG_FILES([bin/env], [chmod a+x bin/env])
AC_CONFIG_FILES([bin/warn-notangle], [chmod a+x bin/warn-notangle])
AC_CONFIG_FILES([bin/namespace-markup], [chmod a+x bin/namespace-markup])
AC_CONFIG_FILES([bin/wrapper], [chmod a+x bin/wrapper])


dnl We can point the example to the build directory.
dnl export EMACSY_CFLAGS="-I${ac_abs_top_builddir}/src/emacsy"
dnl export EMACSY_LIBS="-L${ac_abs_top_builddir}/src/emacsy/.libs"

dnl Autotools are impossible to love.
if test -f configure; then
  abs_top_srcdir="`pwd`";
  abs_top_builddir="`pwd`";
elif test -f ../configure; then
  abs_top_srcdir="`pwd`/..";
  abs_top_builddir="`pwd`";
else
  AC_MSG_ERROR([Can't detect src and build directories.])  
fi

# Unfortunately, since we build the source files using noweb,
# depending on whether it is built in the source tree or out of the
# sourcetree, the header files may or may not be where they ought to
# be in srcdir or builddir.  Bleh.
export EMACSY_CFLAGS="-I${abs_top_srcdir}/src/emacsy -I${abs_top_builddir}/src/emacsy"
export EMACSY_LIBS="-L${abs_top_builddir}/src/emacsy/.libs -lemacsy"
export EMACSY_MODULE="${abs_top_srcdir}/src"
AC_CONFIG_SUBDIRS([example/hello-emacsy])
AC_CONFIG_SUBDIRS([example/emacsy-webkit-gtk])
AC_OUTPUT
