SUBDIRS = emacsy

MAINTAINERCLEANFILES = Makefile.in config.h.in 

NOWEB_FILES = line-pragma.nw

NOWEB_PRODUCTS = line-pragma.scm
CLEANFILES = 
BUILT_SOURCES = 
EXTRA_DIST = convenience-lambda.scm

guilemoduledir = $(prefix)/share/guile/site/
dist_guilemodule_DATA = line-pragma.scm convenience-lambda.scm

include $(top_srcdir)/support/automake/noweb.am
