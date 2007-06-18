#
# Makefile to build all the documentation
#
DOCUMENTATIONTITLE="kadeploy documentation - Documentation for kadeploy deployment system"
OUTDIR=documentation
HTML_ORANGE="--css-include=orange.css"

all: documentation changelog.txt INSTALL

documentation: configuration.texi changelog.texi installation.texi kadeploy.texi kadeploy_scripts.texi man.texi version.texi
	./gendocs.sh -o $(OUTDIR) kadeploy $(DOCUMENTATIONTITLE)
	makeinfo -o kadeploy-info.info kadeploy.texi
	gzip kadeploy-info.info

orange_documentation: configuration.texi changelog.texi installation.texi kadeploy.texi kadeploy_scripts.texi man.texi version.texi
	install -d orange_documentation
	makeinfo -o orange_documentation/html_node --html $(HTML_ORANGE) kadeploy.texi
	makeinfo -o orange_documentation/kadeploy.html --no-split --html $(HTML_ORANGE) kadeploy.texi

changelog.txt: changelog.texi
	makeinfo -o changelog.txt --no-split --no-headers changelog.texi

INSTALL: installation.texi
	makeinfo -o INSTALL --no-split --no-headers installation.texi

clean:
	rm -fr $(OUTDIR) orange_documentation INSTALL changelog.txt kadeploy.aux kadeploy.cp kadeploy.cps kadeploy.fn kadeploy.info kadeploy.ky kadeploy.log kadeploy.pg kadeploy.toc kadeploy.tp kadeploy.vr kadeploy-info.info.gz
