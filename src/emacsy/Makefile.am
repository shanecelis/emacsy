
# Stop make from deleting intermediate files. 
# http://darrendev.blogspot.com/2008/06/stopping-make-delete-intermediate-files.html
.SECONDARY:

NOWEB_DOCS = emacsy.pdf
# Any noweb document can be built by itself as a stand-alone document by
# appending a -paper suffix to its name.  So if you're working on just
# the command.nw, you may want to change NOWEB_DOCS to only make its
# PDF.
#
#NOWEB_DOCS = command-paper.pdf

EXTRA_DIST = $(NOWEB_DOCS)

NOWEB_FILES = emacsy.nw emacsy-c-api.nw windows.nw event.nw util.nw	\
							keymap.nw command.nw buffer.nw block.nw klecl.nw			\
							kbd-macro.nw minibuffer.nw core.nw advice.nw 

SCHEME_TESTS = event-test.scm keymap-test.scm command-test.scm			\
							 buffer-test.scm block-test.scm klecl-test.scm				\
							 kbd-macro-test.scm minibuffer-test.scm core-test.scm	\
							 emacsy-test.scm advice-test.scm

SCHEME_FILES = windows.scm emacsy.c event.scm util.scm keymap.scm	\
							 command.scm buffer.scm block.scm klecl.scm					\
							 kbd-macro.scm minibuffer.scm core.scm emacsy.scm		\
							 advice.scm emacsy.h

NOWEB_PRODUCTS = $(SCHEME_FILES) $(SCHEME_TESTS)				

NOWEB_INCLUDES = $(top_srcdir)/support/noweb/boiler-plate.nw

BUILT_SOURCES = 

CLEANFILES = $(BUILT_SOURCES) command-paper.nw command-paper.tex

doc: $(NOWEB_DOCS)

# The util file may have its chunks specified anywhere.
util.scm : util.nw $(NOWEB_FILES)

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

AM_ETAGSFLAGS = -l scheme
ETAGS_ARGS = $(SCHEME_FILES) $(SCHEME_TESTS)
TAGS_DEPENDENCIES = $(SCHEME_FILES) $(SCHEME_TESTS)

#TESTS_ENVIRONMENT = LOG_COMPILER
TESTS = $(SCHEME_TESTS)
TEST_EXTENSIONS = .scm 
SCM_LOG_COMPILER = $(top_builddir)/bin/runtests

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

emacsy.h emacsy.c : emacsy-c-api.nw

preview-local: $(NOWEB_DOCS)
	open -a Skim.app $^ || open -a Preview.app $^

dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)


