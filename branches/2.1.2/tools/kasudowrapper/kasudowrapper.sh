#!/bin/bash

DEPLOYDIR=/opt/kadeploy/
DEPLOYUSER=deploy
PERL5LIBDEPLOY=$DEPLOYDIR/share/perl/5.8

export PERL5LIB=${PERL5LIBDEPLOY}/:$PERL5LIB
export DEPLOYDIR


if [ -x $DEPLOYDIR/bin/`basename $0` ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ; $OK=1 ;  fi
if [ -x $DEPLOYDIR/sbin/`basename $0` ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ; $OK=1 ;  fi


#if [ $(basename $0) == "setup_pxe.pl" ] ; then  exec sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ; $OK=1 ;  fi
#if [ $OK==0 ]; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ; fi
