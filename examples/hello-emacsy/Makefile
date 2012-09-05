CC = cc

EMACSY_LIB := /Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy/libemacsy.a
GUILE_CFLAGS := $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)
CFLAGS = -g $(GUILE_CFLAGS) -I/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy

LDFLAGS =  -framework OpenGL -framework GLUT $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --libs) 

TARGET = hello-emacsy
VERSION = 0.1

#PDFS = $(TARGET).pdf

LITSRCS = hello-emacsy-paper.nw hello-emacsy.nw

TEXS = hello-emacsy-paper.tex hello-emacsy.tex

DEFS = hello-emacsy-paper.defs hello-emacsy.defs

SRCS = .hello-emacsy hello-emacsy.c

TESTS = 

HDRS = 

OBJS = hello-emacsy.o

BIBS = 

STYS = 

DIST = Makefile README hello-emacsy.nw $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

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

%.defs: %.nw
	nodefs $< > $@

all: $(HDRS) $(SRCS) 
	$(MAKE) $(TARGET)
	$(MAKE) $(TARGET)-paper.pdf

all.defs: $(DEFS)
	sort -u $^ | cpif $@

.hello-emacsy hello-emacsy.scm: hello-emacsy.nw
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

$(TARGET)-paper.pdf: $(TEXS)
	noindex $<
	pdflatex -shell-escape -halt-on-error $<


#preview -r Emacs $(TARGET).pdf

$(TARGET): $(OBJS) $(EMACSY_LIB)

$(TARGET).pdf: hello-emacsy-paper.tex

test: $(SRCS) $(TESTS)
	for test in $(TESTS); do \
		guile -l line-pragma.scm -L . -L .. $$test; \
	done

