SUBDIRS = emacsy

MAINTAINERCLEANFILES = Makefile.in config.h.in 

NOWEB_FILES = line-pragma.nw

NOWEB_PRODUCTS = line-pragma.scm
CLEANFILES = $(NOWEB_CLEANFILES)
BUILT_SOURCES = $(NOWEB_BUILT_FILES)
EXTRA_DIST = convenience-lambda.scm $(NOWEB_EXTRA_DIST)

guilemoduledir = $(prefix)/share/guile/site/
dist_guilemodule_DATA = line-pragma.scm convenience-lambda.scm

include $(top_srcdir)/support/automake/noweb.am
