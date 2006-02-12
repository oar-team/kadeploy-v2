#!/bin/bash

DEPLOYETCDIR="/etc/kadeploy/"
EXPORTENV="exported.conf";
EXPORTENVFILE="$DEPLOYETCDIR/$EXPORTENV"

OK=0
if [ ! -d $DEPLOYETCDIR ] ; then echo "You don't have $DEPLOYETCDIR"; exit 1; fi

if [ -f $EXPORTENVFILE ] ; then 
    source $EXPORTENVFILE ; 
else 
    echo "$EXPORTENVFILE not found... you have to build it with kasetup";
    exit 1;
fi

if [ -z $DEPLOYDIR ] ; then echo "DEPLOYDIR not found in $EXPORTENVFILE" ; exit 1 ; fi
if [ -z $DEPLOYUSER ] ; then echo "DEPLOYUSER not found in $EXPORTENVFILE" ; exit 1 ; fi


export PERL5LIB=${DEPLOYDIR}/:$PERL5LIB
export DEPLOYDIR

if [ $(basename $0) == "kadeploy" ] ; then  exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` -l $USER "$@" ; $OK=1 ;  fi
if [ $(basename $0) == "kaenvironments" ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` -l $USER "$@" ; $OK=1 ; fi
if [ $(basename $0) == "karecordenv" ]   ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` -l $USER "$@" ; $OK=1 ; fi
if [ $(basename $0) == "setup_pxe.pl" ] ; then  exec sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ; $OK=1 ;  fi
if [ $OK==0 ]; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ; fi
