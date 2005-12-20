#!/bin/bash
DEPLOYDIR=/home/local/deploy/kadeploy2
DEPLOYUSER=deploy

export PERL5LIB=${DEPLOYDIR}/:$PERL5LIB
export DEPLOYDIR

exec sudo -u $DEPLOYUSER $DEPLOYDIR/cmd/`basename $0` "$@"

