SRCS=$(wildcard *.pod)
DESTMAN1=$(SRCS:.pod=.1)
CENTERBANNER="Kadeploy commands"

all: $(DESTMAN1)

clean:
	rm -f ../man1/*.1

distclean:
distdir:
install:
	@echo "nothing to do"

distdir:
	@echo "distdir"


%.1: %.pod
	@if ! test -f ../man1/$@ ; then pod2man --center $(CENTERBANNER) $< > ../man1/$@ ; fi
