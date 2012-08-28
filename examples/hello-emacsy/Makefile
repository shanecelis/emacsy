CC = cc

EMACSY_LIB := /Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy/libemacsy.a
GUILE_CFLAGS := $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)

CPPFLAGS = -g  $(GUILE_CFLAGS) -I/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy
LDFLAGS =  -framework OpenGL -framework GLUT $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --libs) 

TARGET = hello-emacsy
VERSION = 0.1

OBJS = hello-emacsy.o

SRCS = hello-emacsy.c .hello-emacsy

HDRS = 

BIBS = 

STYS = 

DIST = Makefile README hello.w $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

%.tex: %.w
	nuweb -lr $<

%.tex: %.nw
	noweave -x -delay $< | cpif $@

%.hw: %.w
	cp $< $@

%.dvi: %.tex
	latex $<

%.pdf: %.tex
	pdflatex -shell-escape $<

%.x: %.c
	guile-snarf -o $@ $(GUILE_CFLAGS) $<

all: $(SRCS)
	$(MAKE) $(TARGET).tex
	$(MAKE) $(TARGET).pdf
	$(MAKE) $(TARGET)

$(SRCS): hello-emacsy.nw
	notangle -R$@ $< | cpif $@

$(HDRS): hello-emacsy.nw
	notangle -R$@ $< | cpif $@

hello-emacsy.o: hello-emacsy.c hello-emacsy.x

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar $(TARGET).pdf

$(TARGET)doc.tex: $(TARGET).tex
	sed -e '/^\\ifshowcode$$/,/^\\fi$$/d' $< > $@

check: nuweb
	@declare -i n=0; \
        declare -i f=0; \
	for i in test/*/*.sh ; do \
	  echo "Testing $$i"; \
	  sh $$i; \
	  if test $$? -ne 0; \
	  then echo "         $$i failed" ; \
	    f+=1; \
	  fi; \
	  n+=1; \
	done; \
        echo "$$n done"; \
        echo "$$f failed"

clean:
	-rm -f *.o *.tex *.log *.dvi *~ *.blg *.lint $(TARGET)

veryclean:
	-rm -f *.o $(SRCS) $(HDRS) $(TARGET).pdf $(TARGET) *.log *.dvi *~ *.blg *.lint *.aux *.bbl *.out

view:	$(TARGET).dvi
	xdvi $(TARGET).dvi

print:	$(TARGET).dvi
	lpr -d $(TARGET).dvi

lint:
	lint $(SRCS) > nuweb.lint

$(OBJS): 

#$(TARGET): $(OBJS)
hello-emacsy: $(OBJS) $(EMACSY_LIB)

upload: hello-emacsy.pdf
	cp hello-emacsy.pdf minimal-emacsy-example.pdf
	scp minimal-emacsy-example.pdf hello-emacsy.c .hello-emacsy emacsy-stub.c emacsy.h gnufoo.org:www/emacsy
