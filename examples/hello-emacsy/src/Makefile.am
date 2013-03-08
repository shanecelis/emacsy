BUILT_SOURCES =
include $(automakedir)/noweb.am
include $(automakedir)/guile.am

CLEANFILES=hello-emacsy-paper.pdf hello-emacsy.c.x $(BUILT_SOURCES)

dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)
NOWEB_SOURCES = hello-emacsy.nw hello-emacsy-paper.nw
BUILT_SOURCES += hello-emacsy.c 
BUILT_SOURCES += $(NOWEB_SOURCES:.nw=.tex)

#hello-emacsy-paper.tex hello-emacsy.tex


# hello-emacsy.c.x



hello-emacsy.c.x : hello-emacsy.c
hello-emacsy.o : hello-emacsy.c.x

all-local : hello-emacsy-paper.pdf

EXTRA_DIST = hello-emacsy-paper.pdf

bin_PROGRAMS = hello-emacsy



AM_CFLAGS  = $(EMACSY_CFLAGS) $(GUILE_CFLAGS)
hello_emacsy_LDADD   = $(EMACSY_LIBS) $(GUILE_LIBS)
hello_emacsy_LDFLAGS = $(GUILE_LDFLAGS) $(GLUT_LDFLAGS) $(GL_LDFLAGS) $(GLU_LDFLAGS)
hello_emacsy_SOURCES = hello-emacsy.c
