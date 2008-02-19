
XML_SRC=install-howto.xml
PDF_OUTPUT=install-howto.pdf
HTML_OUTPUT=install-howto.html

XSL_DOCBOOK_ROOTDIR=/usr/share/xml/docbook/stylesheet
XSL_TLDP=$(XSL_DOCBOOK_ROOTDIR)/ldp/html/tldp-one-page.xsl

XSLTPROC=/usr/bin/xsltproc
HTMLDOC=/usr/bin/htmldoc
HTMLDOC_OPTS=-t pdf14

$(PDF_OUTPUT): $(HTML_OUTPUT)
	       $(HTMLDOC) $(HTMLDOC_OPTS) -f $(PDF_OUTPUT) --webpage $(HTML_OUTPUT)
	       
$(HTML_OUTPUT): $(XML_SRC)
		$(XSLTPROC) -o $(HTML_OUTPUT) $(XSL_TLDP) $?
		
		
