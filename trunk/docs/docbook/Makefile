
XML_FILES=$(wildcard *.xml)
HTML_FILEs=$(XML_FILES:.xml=.html)
PDF_FILES=$(XML_FILES:.xml=.pdf)

PDF_OUTPUT=install-howto.pdf
HTML_OUTPUT=install-howto.html

XSL_DOCBOOK_ROOTDIR=/usr/share/xml/docbook/stylesheet
XSL_TLDP=$(XSL_DOCBOOK_ROOTDIR)/ldp/html/tldp-one-page.xsl
XSLTPROC=/usr/bin/xsltproc

HTMLDOC=/usr/bin/htmldoc
HTMLDOC_OPTS=-t pdf14

RM=/bin/rm

.PHONY: all clean

all: $(HTML_FILES) $(PDF_FILES)

clean: 
	@$(RM) -f *~ *.html *.pdf

%.html: %.xml
	@$(XSLTPROC) -o $@ $(XSL_TLDP) $<

%.pdf: %.html
	@$(HTMLDOC) $(HTMLDOC_OPTS) -f $@ --webpage $<

