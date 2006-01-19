XSLT=xsltproc
SOURCESXML=$(wildcard *.xml)
DESTMAN1=$(SOURCESXML:.xml=.1)
STYLESHEETXML2MAN=../../share/xml-stylesheet/manpages/docbook.xsl

all: $(DESTMAN1)

clean:
	rm -f ../man1/*.1 *~

distclean:

install:
	@echo "nothing to do"

distdir:
	@echo "distdir"


%.1: %.xml
	@if ! test -f ../man1/$@ ; then $(XSLT) -o ../man1/$@ $(STYLESHEETXML2MAN) $< ; fi
