CC = cc

CFLAGS = -g $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)

TARGET = emacsy
VERSION = 0.1

#PDFS = $(TARGET).pdf

LITSRCS = emacsy.nw emacsy-c-api.nw windows.nw event.nw util.nw keymap.nw

TEXS = emacsy.tex emacsy-c-api.tex windows.tex event.tex util.tex keymap.tex

DEFS = emacsy.defs emacsy-c-api.defs windows.defs event.defs util.defs keymap.defs

SRCS = emacsy/windows.scm emacsy.c line-pragma.scm emacsy/event.scm emacsy/util.scm emacsy/keymap.scm

#TESTS = emacsy-tests.scm windows-tests.scm event-tests.scm

TESTS = event-tests.scm keymap-tests.scm

HDRS = emacsy.h

OBJS = emacsy.o

BIBS = 

STYS = 

DIST = Makefile README emacsy.nw $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

.PHONY : all

%.tex: %.nw
	noweave -x -delay $< | cpif $@

%.c: %.nw
	notangle -R$@ $< | cpif $@

%.h: %.nw
	notangle -R$@ $< | cpif $@

%.hw: %.w
	cp $< $@

%.dvi: %.tex
	noindex $<
	latex $<

%.pdf: %.tex
	noindex $<
	pdflatex -shell-escape -halt-on-error $<

%.defs: %.nw
	nodefs $< > $@

all: $(HDRS) $(SRCS) 
	$(MAKE) $(TARGET).pdf
	$(MAKE) lib$(TARGET).a

all.defs: $(DEFS)
	sort -u $^ | cpif $@

emacsy.h emacsy.c: emacsy-c-api.nw
	notangle -R$@ $^ | cpif $@

emacsy-tests.scm: emacsy.nw
	notangle -R$@ $^ | cpif $@

emacsy/windows.scm windows-tests.scm: windows.nw emacsy.nw
	notangle -R$@ $^ | cpif $@

emacsy/event.scm event-tests.scm: event.nw emacsy.nw
	notangle -R$@ $^ | cpif $@

emacsy/keymap.scm keymap-tests.scm: keymap.nw emacsy.nw
	notangle -R$@ $^ | cpif $@

emacsy/util.scm: util.nw event.nw
	notangle -R$@ $^ | cpif $@

line-pragma.scm: emacsy.nw
	notangle -R$@ $^ | cpif $@

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar $(TARGET).pdf 

clean:
	$(RM) *.o $(SRCS) $(HDRS) *.log *.dvi *~ *.blg *.lint $(TARGET).pdf

veryclean: clean
	$(RM) *.aux *.bbl *.out

preview: $(TARGET).pdf
	open -a Skim.app $<

#preview -r Emacs $(TARGET).pdf

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS)

$(TARGET).pdf: $(TEXS)

test: $(SRCS) $(TESTS)
	for test in $(TESTS); do \
		guile -l line-pragma.scm -L . -L .. $$test; \
	done

libemacsy.a: emacsy.o
	ar rcs libemacsy.a emacsy.o
