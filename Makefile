CC = cc

CFLAGS = -g $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)

TARGET = emacsy
VERSION = 0.1

OBJS = emacsy.o

SRCS = emacsy-tests.scm emacsy.c emacsy/windows.scm windows-tests.scm

HDRS = emacsy.h

BIBS = 

STYS = 

DIST = Makefile README emacsy.w $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

.PHONY : all

#%.tex: %.w
#	nuweb -lr $<


%.tex: %.nw
	noweave -x -delay $< | cpif $@

%: %.tex
	latex2html -split 0 $<

%.hw: %.w
	cp $< $@

%.dvi: %.tex
	latex $<

%.pdf: %.tex
	pdflatex -shell-escape -halt-on-error $<


all: $(SRCS) $(HDRS)
	$(MAKE) $(TARGET).tex
	$(MAKE) $(TARGET).pdf
	$(MAKE) lib$(TARGET).a

#$(SRCS): emacsy.w
#	nuweb -t $<

$(SRCS) $(HDRS): emacsy.nw
	notangle -R$@ $< | cpif $@

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar hello.pdf 

$(TARGET)doc.tex: $(TARGET).tex
	sed -e '/^\\ifshowcode$$/,/^\\fi$$/d' $< > $@


clean:
	-rm -f *.o emacsy.tex *.log *.dvi *~ *.blg *.lint $(TARGET)

veryclean:
	-rm -f *.o *.c *.h *.log *.dvi *~ *.blg *.lint *.aux *.bbl *.out

view:	$(TARGET).dvi
	xdvi $(TARGET).dvi

print:	$(TARGET).dvi
	lpr -d $(TARGET).dvi

preview: $(TARGET).pdf
	preview -r Emacs $(TARGET).pdf

lint:
	lint $(SRCS) > nuweb.lint

$(OBJS): 

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS)

test: emacsy-tests.scm windows-tests.scm
	guile -l line-pragma.scm -L . -L .. emacsy-tests.scm
	guile -l line-pragma.scm -L . -L .. windows-tests.scm

libemacsy.a: emacsy.o
	ar rcs libemacsy.a emacsy.o
