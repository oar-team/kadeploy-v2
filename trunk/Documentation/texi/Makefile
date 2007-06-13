#
# Makefile to build all the documentation
#
DOCUMENTATIONTITLE="kadeploy documentation - Documentation for kadeploy deployment system"
OUTDIR=documentation

all: documentation changelog.txt INSTALL

documentation: configuration.texi changelog.texi installation.texi kadeploy.texi man.texi version.texi
	./gendocs.sh -o $(OUTDIR) kadeploy $(DOCUMENTATIONTITLE)

changelog.txt: changelog.texi
	makeinfo -o changelog.txt --no-split --no-headers changelog.texi

INSTALL: installation.texi
	makeinfo -o INSTALL --no-split --no-headers installation.texi

clean:
	rm -fr $(OUTDIR) INSTALL changelog.txt kadeploy.aux kadeploy.cp kadeploy.cps kadeploy.fn kadeploy.info kadeploy.ky kadeploy.log kadeploy.pg kadeploy.toc kadeploy.tp kadeploy.vr
