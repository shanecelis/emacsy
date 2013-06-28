
# Stop make from deleting intermediate files. 
# http://darrendev.blogspot.com/2008/06/stopping-make-delete-intermediate-files.html
.SECONDARY:

NOWEB_DOCS = emacsy.pdf 
EXTRA_DIST = $(NOWEB_DOCS)

NOWEB_FILES = emacsy.nw emacsy-c-api.nw windows.nw event.nw util.nw	\
							keymap.nw command.nw buffer.nw block.nw klecl.nw			\
							kbd-macro.nw minibuffer.nw core.nw advice.nw line-pragma.nw

SCHEME_TESTS = event-test.scm keymap-test.scm command-test.scm			\
							 buffer-test.scm block-test.scm klecl-test.scm				\
							 kbd-macro-test.scm minibuffer-test.scm core-test.scm	\
							 emacsy-test.scm advice-test.scm

SCHEME_FILES = windows.scm emacsy.c line-pragma.scm event.scm				\
							 util.scm keymap.scm command.scm buffer.scm block.scm	\
							 klecl.scm kbd-macro.scm minibuffer.scm core.scm			\
							 emacsy.scm advice.scm emacsy.h

NOWEB_PRODUCTS = $(SCHEME_FILES) $(SCHEME_TESTS)				

BUILT_SOURCES = 
#emacsy.c.x

CLEANFILES = $(BUILT_SOURCES)

doc: $(NOWEB_DOCS)

if LATEXMK
all-local : $(NOWEB_DOCS)
CLEANFILES += $(NOWEB_DOCS)

ctrnn.tex: ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
#ctrnn-appendix.tex: ctrnn-appendix.nw ctrnn.nw $(BOILERPLATE)

ctrnn-paper.pdf: ctrnn-paper.tex ctrnn.tex
vision-paper.pdf: vision-paper.tex vision.tex

util.scm : util.nw $(NOWEB_FILES)


# ctrnn-cat.nw: ctrnn.nw ctrnn-appendix.nw
# 	cat $^ | cpif $@

endif
clean-local : clean-latexmk

MARKUP_FLAGS = -markup "$(top_srcdir)/bin/namespace-markup.pl -n ctrnn-appendix=ctrnn -n ctrnn-cat=ctrnn --"
#MARKUP_FLAGS = -markup "$(top_srcdir)/bin/namespace-markup.pl --"
#WARN_NOTANGLE_FLAGS = -W # treat warnings as errors
WARN_NOTANGLE_FLAGS = 
NOWEAVE_FLAGS = $(MARKUP_FLAGS)
NOTANGLE_C_FLAGS = -c -L $(MARKUP_FLAGS)
NOTANGLE_LISP_FLAGS = -L $(MARKUP_FLAGS)
NOTANGLE_H_FLAGS = -L $(MARKUP_FLAGS)
lib_LTLIBRARIES = libemacsy.la

AM_CFLAGS = $(GUILE_CFLAGS)
libemacsy_la_LIBADD  = $(GUILE_LIBS)
libemacsy_la_SOURCES = emacsy.c
include_HEADERS = emacsy.h 

MAINTAINERCLEANFILES = Makefile.in config.h.in 

guilemoduledir = $(GUILE_SITE_LIB)/$(PACKAGE_TARNAME)
dist_guilemodule_DATA = $(SCHEME_FILES)


# check_PROGRAMS = ctrnn-ctest vision-ctest
# ctrnn_ctest_LDADD   = libguile-ctrnn.la
# ctrnn_ctest_SOURCES = ctrnn-test.c


#TESTS_ENVIRONMENT = $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme -x $(libdir)
#TESTS_ENVIRONMENT = LOG_COMPILER
TESTS = $(SCHEME_TESTS)
TEST_EXTENSIONS = .scm 
SCM_LOG_COMPILER = $(top_builddir)/bin/runtests

# TESTS_ENVIRONMENT = 
# TESTS =  ctrnn-test vision-test 

# src_NOWEB = ctrnn.c 
# #ctrnn.scm ctrnn.h
# ctrnn_c_SOURCES = ctrnn.nw ctrnn-appendix.nw

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am
#include $(top_srcdir)/support/automake/noweb2.am

# ctrnn.c.x : ctrnn.c
# vision.c.x : vision.c
# fode.c.x : fode.c

emacsy.h emacsy.c : emacsy-c-api.nw

# ctrnn.c : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
# ctrnn-test.scm : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
# ctrnn-test.c : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)

# vision.c : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
# vision.h : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
# vision-test.c : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
# vision-test.scm : vision.nw ctrnn-appendix.nw $(BOILERPLATE)

# fode-test.scm : fode.nw  $(BOILERPLATE)


dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)


