#START MODIFVARIABLE <![CDATA[
XSLT=xsltproc
FOP=fop.sh
CONVERT=convert
VIEWPDF=xpdf
VIEWHTML=links
ECHO=echo
DESTGRAPHICFORMAT=jpg
XSLDIR=/usr/share/sgml/docbook/stylesheet/xsl/nwalsh
XSLHTMLONEPAGE=$(XSLDIR)/html/docbook.xsl
XSLHTMLCHUNK=$(XSLDIR)/html/chunk.xsl
XSLFO=$(XSLDIR)/fo/docbook.xsl
GRAPHIC=graphics
INCLUDE=chapitre
SUFFIXINCLUDE=pm xml
DATADIR=data
OUT=publication
ENCODING="ISO-8859-1"
DTD="/usr/share/sgml/docbook/dtd/4.3/docbookx.dtd"
FPI="-//OASIS//DTD DocBook XML V4.3//EN"
HEADERSTART="<?xml version='1.0' encoding=\"$(ENCODING)\" ?> <!DOCTYPE book PUBLIC \"$(FPI)\" \"$(DTD)\""
FIRSTNAME="Peyrard"
LASTNAME="Johann"
INITIALS="J.P"
DATE=`date`
#END MODIFVARIABLE

SOURCEGRAPHIC=$(DATADIR)
SOURCEGRAPHICS=$(wildcard $(DATADIR)/*.png)
T1DESTGRAPHICS=$(subst .png,.jpg,$(SOURCEGRAPHICS))
DESTGRAPHICS=$(subst $(DATADIR),$(GRAPHIC),$(T1DESTGRAPHICS))
SOURCESXML=$(wildcard *.xml)
SOURCESSGML=$(wildcard *.sgml)  
SOURCES=$(SOURCESXML) $(SOURCESSGML)
SOURCESXMLWITHOUTSUFFIX=$(SOURCESXML:.xml=)
SOURCESGMLWITHOUTSUFFIX=$(SOURCESSGML:.sgml=)
SOURCESWITHOUTSUFFIX=$(SOURCESXMLWITHOUTSUFFIX) $(SOURCESGMLWITHOUTSUFFIX)

SOURCEINCLUDES=$(wildcard $(INCLUDE)/* )
GRAPHICS=$(wildcard $(GRAPHIC)/*)

OUTHTML=$(OUT)/html/
OUTHTMLCHUNK=$(addprefix $(OUTHTML),$(SOURCESWITHOUTSUFFIX))
OUTPDF=$(OUT)/pdf


#DTD="/usr/share/sgml/docbook/dtd/xml/4.3/docbookx.dtd"
#FPI="-//OASIS//DTD DocBook XML V4.3//EN"


HTML=$(SOURCESXML:.xm l=.html) $(SOURCESSGML:.sgml=.html)
HTMLCHUNK=$(addprefix $(OUTHTML)/,$(SOURCESWITHOUTSUFFIX))
PDF=$(SOURCESXML:.xml=.pdf)   $(SOURCESSGML:.sgml=.pdf)
FO=$(SOURCESXML:.xml=.fo)     $(SOURCESSGML:.sgml=.fo)

PDFOUTPUT=$(addprefix  $(OUTPDF),$(PDF))
HTMLOUTPUT=$(addprefix $(OUTHTML),$(HTML))



entete:
	@$(ECHO)
	@$(ECHO) "Publication XSL2HTML XSL2PDF "
	@$(ECHO) "------------------------------"
	@$(ECHO)
	@$(ECHO) "DTD             :" `echo $(DTD)`
	@$(ECHO) "XSLHTMLONEPAGE  :" `echo $(XSLHTMLONEPAGE)`
	@$(ECHO) "XSLHTMLCHUNK    :" `echo $(XSLHTMLCHUNK)`
	@$(ECHO) "XSLFO           :" `echo $(XSLFO)`
	@$(ECHO) "XSLT            :" `which $(XSLT)`
	@$(ECHO) "FOP             :" `which $(FOP)`
	@$(ECHO) "CONVERT         :" `which $(CONVERT)`
	@$(ECHO) ""
	@$(ECHO) "SOURCE          :" $(SOURCES)
	@$(ECHO) "INCLUDE         :" $(INCLUDE)
	@$(ECHO) "GRAPHIC SOURCE  :" $(SOURCEGRAPHIC) 
	@$(ECHO)
	@$(ECHO) "OUTHTML         :" `pwd`"/"`echo $(OUTHTML)`
	@$(ECHO) "OUTPDF          :" `pwd`"/"`echo $(OUTPDF)`
	@$(ECHO)

	@$(ECHO) "make help       : pour les benets"
	@$(ECHO) 
#	@$(ECHO) $(SOURCESWITHOUTSUFFIX)
#	@$(ECHO) $(OUTHTMLCHUNK)
#	@$(ECHO) "DESTGRAPHICS    : "$(DESTGRAPHICS)
#	@$(ECHO) "SOURCEGRAPHICS  : "$(SOURCEGRAPHICS)

help:
	@$(ECHO) ""
	@$(ECHO) "Help"
	@$(ECHO) "----"
	@$(ECHO) ""
	@$(ECHO) "xmlhelp      Page principal"
	@$(ECHO) "xmlhelp2     Chapitre"
	@$(ECHO) ""
	@$(ECHO) "html         1 page et les chunks"
	@$(ECHO) "onepage      1 page"
	@$(ECHO) "chunk        les chunks"
	@$(ECHO) "pdf          le pdf"
	@$(ECHO) ""
	@$(ECHO) "viewhtml     html + #$(VIEWHTML) $(HTML)"
	@$(ECHO) "viewpdf      pdf  + #$(VIEWPDF) $(PDF)"
	@$(ECHO) ""

xmlhelp: directory
	@$(ECHO) $(HEADERSTART) "[" 
	@$(ECHO) 
	@$(ECHO) "<!--Header-->"
	@( cd $(INCLUDE) ; for suf in $(SUFFIXINCLUDE) ; do echo "" ; for i in `ls *$$suf 2>/dev/null` ; do if [ ! -z $$i ]; then j=`echo $$i | sed -e s/.$$suf//`; echo -n '<!ENTITY ' ; echo -n $$j ; echo -n ' SYSTEM "' ; echo -n "$(INCLUDE)/" ; echo -n $$i ; echo -n '">'; echo "" ; fi ; done ; done )
	@$(ECHO) "]>"
	@$(ECHO) 
	@$(ECHO) "<book lang=\"fr\">"
	@$(ECHO) "<!--Info-->"
	@$(ECHO) "<bookinfo>"
	@$(ECHO) " <author><firstname>" $(FIRSTNAME) "</firstname><lastname>" $(LASTNAME) "</lastname></author>"
	@$(ECHO) "  <date>" $(DATE) "</date>"
	@$(ECHO) "<!--Versioning-->"
	@$(ECHO) " <revhistory>"
	@$(ECHO) "    <revision>"
	@$(ECHO) "      <revnumber>1</revnumber>"
	@$(ECHO) "      <date>" $(DATE) "</date>"
	@$(ECHO) "      <authorinitials>J.Peyrard</authorinitials>"
	@$(ECHO) "      <revremark>Création de structure de document</revremark> "
	@$(ECHO) "    </revision>"
	@$(ECHO) " </revhistory>"
	@$(ECHO) "</bookinfo>"
	@$(ECHO) "<!--Chapter-->"
	@$(ECHO) "&chap01;"
	@$(ECHO) "</book>"
	@$(ECHO)

xmlhelp2:
	@$(ECHO) "<?xml version=\"1.0\" encoding=\"$(ENCODING)\" ?>"
	@$(ECHO) "<chapter id=\"chap01\"><title>Mon premier chapitre</title>"
	@$(ECHO) "<sect1> <title> N'oubliez pas de prendre une serviette!!! </title> </sect1>"
	@$(ECHO) "</chapter>"


all: $(HTML) $(PDF) viewhtml


viewpdf: 
	find . -name "*.pdf" | grep $(SOURCESWITHOUTSUFFIX)  | xargs  $(VIEWPDF)


viewhtml: 
	find . -name "index.html" | grep $(SOURCESWITHOUTSUFFIX) | xargs $(VIEWHTML)

directory:  
	@mkdir -p $(GRAPHIC)
	@mkdir -p $(DATADIR)
	@mkdir -p $(OUTHTML)
	@mkdir -p $(OUTPDF)
	@if [ $(OUTHTMLCHUNK) ]; then mkdir -p $(OUTHTMLCHUNK) ; fi
	@mkdir -p $(INCLUDE)
	@for G in $(GRAPHICS) ; do if [ -n "$$G" ]; then ln -f $$G $(OUTHTML)/  ; fi ; done
	@for G in $(GRAPHICS) ; do if [ -n "$$G" ]; then ln -f $$G $(OUTPDF)/  ; fi ; done
	@for G in $(GRAPHICS) ; do if [ -n "$$G" ]; then for Y in $(OUTHTMLCHUNK) ; do ln -f $$G $$Y  ; done ; fi ; done 


html: directory  $(HTML) onepage chunk convert

onepage: convert directory $(HTML)

chunk: convert directory $(HTML)
	for X in $(SOURCESXML) ; do DIR=`echo $(OUTHTML)/$$X | sed 's/.xml$///' ` ; echo $$DIR/ ; $(XSLT) -o $$DIR/   $(XSLHTMLCHUNK) $$X  ; done
	for X in $(SOURCESSGML) ; do DIR=`echo $(OUTHTML)/$$X | sed 's/.sgml$///' ` ; echo $$DIR/ ; $(XSLT) -o $$DIR/   $(XSLHTMLCHUNK) $$X  ; done

pdf: directory $(PDF)

fo: $(FO)

convert: 
	@format=png ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@format=jpg ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@format=eps ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@format=ps  ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@format=gif ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@format=fig ; cd $(DATADIR) ; if [ `ls *.$$format 2>/dev/null | wc -l` != 0 ]; then for i in *$$format ; do j=`echo $$i | sed -e s/$$format/$(DESTGRAPHICFORMAT)/`; if [ ! -e ../graphics/$$j ] ; then echo "converting $$i => $$j" ;  cat $$i | convert - ../graphics/$$j ; fi done ; fi
	@echo "convert done..."

%.pdf: %.fo
	$(FOP) $(OUTPDF)/$< $(OUTPDF)/$@

%.fo: $(SOURCESXML) $(SOURCESSGML)
	$(XSLT) $(XSLFO) $< >  $(OUTPDF)/$@

%.html: %.sgml
	$(XSLT) $(XSLHTMLONEPAGE) $< > $(OUTHTML)/$@

%.html: %.xml
	$(XSLT) $(XSLHTMLONEPAGE) $< > $(OUTHTML)/$@

clean:
	rm -f *.fo *.pdf *.html
	rm -rf $(OUTHTML) $(OUTPDF)
