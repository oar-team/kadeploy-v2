#!/bin/bash

DEPLOYDIR=/usr/local/kadeploy/
DEPLOYUSER=deploy
PERL5LIBDEPLOY=$DEPLOYDIR/share/perl/5.8

export PERL5LIB=${PERL5LIBDEPLOY}/:$PERL5LIB
export DEPLOYDIR

append=""

if [ $(basename $0) = "kadeploy" ]
	then if $(tty -s)
		then
			append=${append}
		else
			append="screen -D -m "
			echo -e "Kadeploy not launched from a tty\nDetached in a screen\n "
	fi
fi

#if [ -x $DEPLOYDIR/bin/`basename $0` ] ; then exec ${append} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ; $OK=1 ;  fi
#if [ -x $DEPLOYDIR/sbin/`basename $0` ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ; $OK=1 ;  fi
#if [ ! $OK ] ; then echo "kasudowrapper.sh badly configured, use (prefix)/sbin/kasetup -exportenv" ; fi

if [ -x $DEPLOYDIR/bin/`basename $0` ]
then 
    ${append} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@"
    if [ "`basename $0`" == "kadeploy" ]
    then
	cp /tmp/kadeploy-$USER*.out .
	sudo -u $DEPLOYUSER $DEPLOYDIR/bin/kadeploy -rmnodefilesintmp $USER
    fi
fi
if [ -x $DEPLOYDIR/sbin/`basename $0` ]
then
    sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" 
fi

