SOURCES=$(wildcard *.tex *.sgml)
TEXTOHTML=$(SOURCES:.tex=.html)
SGMLTOHTML=$(SOURCES:.sgml=.html)

OUTPUTDIR=../html/

all: mkoutput $(TEXTOHTML) $(SGMLTOHTML)

mkoutput:
	mkdir -p $(OUTPUTDIR)

distclean:

install:

distdir:

clean:
	rm -f *.aux *.dvi *.toc *.log
	rm -rf ../html/*

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

%.html : %.tex
	if test ! -d $(OUTPUTDIR)/$</ ; then latex2html -split 0 $< -mkdir -dir $(OUTPUTDIR)/$</ ; fi

%.tex : %.sgml
	if test ! -d $(OUTPUTDIR)/$</ ; then linuxdoc -B latex $< ; fi
