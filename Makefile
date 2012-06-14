CC = cc

CFLAGS = -g

TARGET = emacsy
VERSION = 0.1

OBJS = 

SRCS = emacsy.scm emacsy-tests.scm

HDRS = 

BIBS = 

STYS = 

DIST = Makefile README emacsy.w $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

%.tex: %.w
	nuweb -lr $<

%: %.tex
	latex2html -split 0 $<

%.hw: %.w
	cp $< $@

%.dvi: %.tex
	latex $<

%.pdf: %.tex
	pdflatex -shell-escape $<

$(SRCS): emacsy.w
	nuweb -t $<

all: $(SRCS)
	$(MAKE) $(TARGET).tex
	$(MAKE) $(TARGET).pdf

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar hello.pdf 

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
	-rm -f *.o *.c *.h *.tex *.log *.dvi *~ *.blg *.lint *.aux *.pdf *.bbl *.out

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

test: emacsy-tests.scm
	guile -l line-pragma.scm -L . emacsy-tests.scm
