#
## Makefile
#
PROGRAM1=mcatseg
SOURCES1= mcatseg.c CreateTCPServerSocket.c AcceptTCPConnection.c DieWithError.c
OBJECTS1=$(wildcard *.o)
INCLUDES= TCPCatServer.h
CFLAGS= -static
SLIBS= 
CC=gcc
OBJECTS= $(SOURCES.c:.c=.o)
OBJECTS2= $(SOURCES2.c:.c=.o)
.KEEP_STATE:

debug := CFLAGS= 

all: $(PROGRAM1)

$(PROGRAM1):$(SOURCES1)
	$(CC) $(CFLAGS) -o $@ $(SOURCES1)

clean:
	rm -f $(PROGRAM1) $(OBJECTS1)

install:
	cp $(PROGRAM1) $(DESTDIR)

distdir:
