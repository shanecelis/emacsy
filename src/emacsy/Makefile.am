
# Stop make from deleting intermediate files. 
# http://darrendev.blogspot.com/2008/06/stopping-make-delete-intermediate-files.html
.SECONDARY:

NOWEB_DOCS = _emacsy.pdf
# Any noweb document can be built by itself as a stand-alone document by
# appending a -paper suffix to its name.  So if you're working on just
# the command.nw, you may want to change NOWEB_DOCS to only make its
# PDF.
#
#NOWEB_DOCS = command-paper.pdf

NOWEAVE_HEADER = $(top_srcdir)/support/noweb/paper-header.nw
NOWEAVE_FOOTER = $(top_srcdir)/support/noweb/paper-footer.nw

PARTIAL_TEX = _emacsy-c-api.tex _event.tex _keymap.tex _command.tex _block.tex _klecl.tex _advice.tex _buffer.tex _minibuffer.tex _core.tex _window.tex _util.tex
_emacsy.pdf : $(PARTIAL_TEX)

# XXX this rule that pulls something from one of the examples is not working.
# I'm going to just copy the file over for the meantime.  Ugh.

# hello-emacsy.tex : $(helloemacsydir)/src/hello-emacsy.nw
# 	$(MAKE) -C $(helloemacsydir) all
# 	cp $(helloemacsydir)/src/hello-emacsy.tex hello-emacsy.tex

EXTRA_DIST = $(SCHEME_TESTS) $(NOWEB_FILES) 

NOWEB_FILES = emacsy.nw emacsy-c-api.nw event.nw util.nw keymap.nw \
							command.nw buffer.nw block.nw klecl.nw kbd-macro.nw	 \
							minibuffer.nw core.nw advice.nw window.nw help.nw self-doc.nw mru-stack.nw

NOWEB_SCHEME_TESTS = event-test.scm self-doc-test.scm keymap-test.scm	\
 command-test.scm buffer-test.scm block-test.scm klecl-test.scm				\
 kbd-macro-test.scm core-test.scm emacsy-test.scm advice-test.scm			\
 minibuffer-test.scm window-test.scm help-test.scm mru-stack-test.scm

NOWEB_SCHEME_FILES = event.scm util.scm self-doc.scm keymap.scm		 \
 command.scm buffer.scm block.scm klecl.scm kbd-macro.scm					 \
 minibuffer.scm core.scm emacsy.scm advice.scm window.scm help.scm mru-stack.scm

SCHEME_FILES = $(NOWEB_SCHEME_FILES) coroutine.scm agenda.scm mode.scm job.scm

SCHEME_TESTS = $(NOWEB_SCHEME_TESTS) job-test.scm

GOBJECTS = $(SCHEME_FILES:%.scm=%.go) 

NOWEB_PRODUCTS = $(NOWEB_SCHEME_FILES) $(NOWEB_SCHEME_TESTS) emacsy.h emacsy.c

NOWEB_INCLUDES = $(top_srcdir)/support/noweb/boiler-plate.nw

BUILT_SOURCES = $(NOWEB_PRODUCTS)

CLEANFILES = $(NOWEB_CLEANFILES) $(GOBJECTS)

distdir-local: 
	$(MAKE) all

doc: $(NOWEB_DOCS)

# The util file may have its chunks specified anywhere.
util.scm : util.nw $(NOWEB_FILES)

if LINE_PRAGMA
LINE_PRAGMA_FLAGS = -L
else
LINE_PRAGMA_FLAGS = 
endif

MARKUP_FLAGS = -markup "$(top_builddir)/bin/namespace-markup --"
#WARN_NOTANGLE_FLAGS = -W # treat warnings as errors
WARN_NOTANGLE_C_FLAGS = 
WARN_NOTANGLE_LISP_FLAGS = $(LINE_PRAGMA_FLAGS) # add a (use-modules (line-pragma)) header
NOWEAVE_FLAGS = $(MARKUP_FLAGS)
NOTANGLE_C_FLAGS = -c $(LINE_PRAGMA_FLAGS) $(MARKUP_FLAGS)
NOTANGLE_LISP_FLAGS = $(LINE_PRAGMA_FLAGS) $(MARKUP_FLAGS)
NOTANGLE_H_FLAGS = $(LINE_PRAGMA_FLAGS) $(MARKUP_FLAGS)
lib_LTLIBRARIES = libemacsy.la

AM_CFLAGS = $(GUILE_CFLAGS)
libemacsy_la_LIBADD  = $(GUILE_LIBS)
libemacsy_la_SOURCES = emacsy.c
include_HEADERS = emacsy.h 

DISTCLEANFILES = $(PARTIAL_TEX) _emacsy.tex
MAINTAINERCLEANFILES = Makefile.in config.h.in 

guilemoduledir = $(prefix)/share/guile/site/$(PACKAGE_TARNAME)
dist_guilemodule_DATA = $(SCHEME_FILES) $(GOBJECTS)

AM_ETAGSFLAGS = -l scheme
ETAGS_ARGS = $(SCHEME_FILES) $(SCHEME_TESTS)
TAGS_DEPENDENCIES = $(SCHEME_FILES) $(SCHEME_TESTS)

#TESTS_ENVIRONMENT = LOG_COMPILER
TESTS = $(SCHEME_TESTS)
TEST_EXTENSIONS = .scm 
SCM_LOG_COMPILER = $(top_builddir)/bin/env $(GUILE)

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

emacsy.h emacsy.c : emacsy-c-api.nw

dist-hook: _emacsy.pdf
	cp $(builddir)/_emacsy.pdf $(top_distdir)/emacsy.pdf


