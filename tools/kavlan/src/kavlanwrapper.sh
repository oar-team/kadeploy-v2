#!/bin/bash

USERNAME=`id -u -n`
KAVLANUSER=kavlan
KAVLANSERV=services
export KAUSER=$USERNAME
sudo -u $KAVLANUSER /usr/local/sbin/kavlan-front.pl $*
