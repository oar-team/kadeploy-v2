#!/bin/bash

DEPLOYDIR=$( cat /etc/kadeploy/deploy.conf  | grep kadeploy2_directory | awk -F = '{print $2}' )
DEPLOYUSER=$( cat /etc/kadeploy/deploy.conf  | grep deploy_user | awk -F = '{print $2}' )

if [ -z $DEPLOYDIR ] ; then echo "kadeploy2_directory not found in your configuration files" ; exit 1 ; fi
if [ -z $DEPLOYUSER ] ; then echo "deploy_user not found in your configuration files" ; exit 1 ; fi


export PERL5LIB=${DEPLOYDIR}/:$PERL5LIB
export DEPLOYDIR

if [ $(basename $0) == "kaenvironments" ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` -l $USER "$@" ; 
else
	if [ $(basename $0) == "karecordenv" ]   ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` -l $USER "$@" ; 
	else
	exec sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@"
	fi
fi
