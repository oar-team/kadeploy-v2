SRCS=$(wildcard *.pod)
DESTMAN1=$(SRCS:.pod=.1)
DESTTEXI=$(SRCS:.pod=.texi)
CENTERBANNER="Kadeploy commands"

all: $(DESTMAN1) $(DESTTEXI)

clean:
	rm -f ../man1/*.1 ../texi/* ./*.tmp

distclean:
distdir:
install:
	@echo "nothing to do"

distdir:
	@echo "distdir"


%.1: %.pod
	@if ! test -f ../man1/$@ ; then pod2man --section=1 --center $(CENTERBANNER) $< > ../man1/$@ ; fi

%.texi: %.pod
	@if ! test -f ../texi/$@ ; then cat $< | sed -f pod2texi.sed > ../texi/$@ ; fi
