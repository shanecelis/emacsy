BUILT_SOURCES =
include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

CLEANFILES=hello-emacsy.c.x

if LATEXMK
CLEANFILES+=hello-emacsy-paper.pdf
endif

dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)

NOWEB_FILES = hello-emacsy.nw hello-emacsy-paper.nw
EXTRA_DIST = hello-emacsy-paper.pdf

if NOWEB
EXTRA_DIST += $(NOWEB_FILES:.nw=.tex)
BUILT_SOURCES += hello-emacsy.c 
BUILT_SOURCES += $(NOWEB_FILES:.nw=.tex)
CLEANFILES += $(NOWEB_FILES:.nw=.tex) hello-emacsy.c 
# Maybe I should call latexmk -C before removing the .tex files.
endif

#hello-emacsy-paper.tex hello-emacsy.tex
# hello-emacsy.c.x
hello-emacsy.c.x : hello-emacsy.c
hello-emacsy.o : hello-emacsy.c.x

if LATEXMK
all-local : hello-emacsy-paper.pdf
endif

bin_PROGRAMS = hello-emacsy

AM_CFLAGS  = $(EMACSY_CFLAGS) $(GUILE_CFLAGS)
hello_emacsy_LDADD   = $(EMACSY_LIBS) $(GUILE_LIBS)
hello_emacsy_LDFLAGS = $(GUILE_LDFLAGS) $(GLUT_LDFLAGS) $(GL_LDFLAGS) $(GLU_LDFLAGS)
hello_emacsy_SOURCES = hello-emacsy.c

MAINTAINERCLEANFILES = Makefile.in config.h.in 
