CC = cc

CFLAGS = -g $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)

TARGET = emacsy
VERSION = 0.1

#PDFS = $(TARGET).pdf

LITSRCS = emacsy.nw emacsy-c-api.nw windows.nw event.nw util.nw keymap.nw examples/hello-emacsy/hello-emacsy.nw command.nw buffer.nw block.nw klecl.nw kbd-macro.nw minibuffer.nw core.nw

TEXS = emacsy.tex emacsy-c-api.tex windows.tex event.tex util.tex keymap.tex examples/hello-emacsy/hello-emacsy.tex command.tex buffer.tex block.tex klecl.tex kbd-macro.tex minibuffer.tex core.tex

DEFS = emacsy.defs emacsy-c-api.defs windows.defs event.defs util.defs keymap.defs examples/hello-emacsy/hello-emacsy.defs command.defs buffer.defs block.defs klecl.defs kbd-macro.defs minibuffer.defs core.defs

SRCS = emacsy/windows.scm emacsy.c line-pragma.scm emacsy/event.scm emacsy/util.scm emacsy/keymap.scm emacsy/command.scm emacsy/buffer.scm emacsy/block.scm emacsy/klecl.scm emacsy/kbd-macro.scm emacsy/minibuffer.scm emacsy/core.scm emacsy/emacsy.scm check/harness.scm

TESTS = event-tests.scm  \
        keymap-tests.scm command-tests.scm buffer-tests.scm \
        block-tests.scm klecl-tests.scm kbd-macro-tests.scm \
        minibuffer-tests.scm core-tests.scm emacsy-tests.scm

#windows-tests.scm
#TESTS = core-tests.scm

HDRS = emacsy.h

OBJS = emacsy.o

BIBS = 

STYS = 

DIST = Makefile README emacsy.nw $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

GRAPHICS_PATH = examples/hello-emacsy

NOTANGLE_LISP_FLAGS = -L'\#line %L "%F"%N' -filter 'docs2comments -one -scm' 

.PHONY : all

%.tex: %.nw all.defs
	noweave -n -delay -indexfrom all.defs $< | cpif $@

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
	TEXINPUTS=.:$(GRAPHICS_PATH): pdflatex -shell-escape -halt-on-error $<
	noindex $<
	TEXINPUTS=.:$(GRAPHICS_PATH): pdflatex -shell-escape -halt-on-error $<

# Where should all the global defs go?
%.defs: %.nw
	nodefs $< > $@

all: 
	$(MAKE) lib$(TARGET).a
	$(MAKE) source

doc: 
	$(MAKE) $(TARGET).pdf


source: $(HDRS) $(SRCS)

all.defs: $(DEFS)
	sort -u $^ | cpif $@

emacsy.h emacsy.c: emacsy-c-api.nw
	notangle -R$@ $^ | cpif $@

emacsy/emacsy.scm emacsy-tests.scm check/harness.scm: emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/windows.scm windows-tests.scm: windows.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/util.scm: util.nw event.nw keymap.nw buffer.nw block.nw klecl.nw minibuffer.nw core.nw command.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/event.scm event-tests.scm: event.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/keymap.scm keymap-tests.scm: keymap.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/command.scm command-tests.scm: command.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/buffer.scm buffer-tests.scm: buffer.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/block.scm block-tests.scm: block.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/klecl.scm klecl-tests.scm: klecl.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/kbd-macro.scm kbd-macro-tests.scm: kbd-macro.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/minibuffer.scm minibuffer-tests.scm: minibuffer.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

emacsy/core.scm core-tests.scm: core.nw emacsy.nw
	notangle $(NOTANGLE_LISP_FLAGS) -R$@ $^ | cpif $@

line-pragma.scm: emacsy.nw
	notangle -R$@ $^ | cpif $@

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar $(TARGET).pdf 

clean:
	$(RM) $(OBJS) $(SRCS) $(TESTS) $(HDRS) $(TEXS) $(DEFS) all.defs *.log *.dvi *~ *.blg *.lint $(TARGET).pdf

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
		guile -l line-pragma.scm -L . $$test || exit 1; \
	done

emacsy.o: emacsy.h

libemacsy.a: emacsy.o
	ar rcs libemacsy.a emacsy.o

TAGS: $(SRCS) $(TESTS)
	etags -l scheme $(SRCS) $(TESTS)
