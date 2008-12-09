#!/bin/bash

DIR=debootstrap
SCRIPTS_DIR=scripts

DEBOOTSTRAP="/usr/sbin/debootstrap"
DEBOOTSTRAP_INCLUDE_PACKAGES=dhcpcd,rsh-server,rsh-client,kexec-tools,bzip2,configure-debian

DEBOOTSTRAP_EXCLUDE_PACKAGE=openssh-client,vim-common,vim-tiny,traceroute,manpages,man-db,adduser,cron,logrotate,laptop-detect,tasksel,tasksel-data,dhcp3-client,dhcp3-common,wget


mkdir -p $DIR

$DEBOOTSTRAP --include=$DEBOOTSTRAP_INCLUDE_PACKAGES --exclude=$DEBOOTSTRAP_EXCLUDE_PACKAGE etch $DIR

echo "127.0.0.1       localhost" > $DIR/etc/hosts

echo "localhost" >  $DIR/etc/hostname

cat > $DIR/etc/passwd <<EOF
root::0:0:Da r00t:/:/bin/bash
EOF

echo "+ +" > $DIR/.rhosts
chmod 777 $DIR/.rhosts

echo "rsh" >> $DIR/etc/securetty

cat > $DIR/etc/pam.d/rsh <<EOF
auth       sufficient	pam_rhosts_auth.so
auth       required	pam_rootok.so
EOF

mv $DIR/sbin/init $DIR/sbin/true_init 

cp init $DIR/sbin/init
cp linuxrc $DIR/

cp $SCRIPTS_DIR/* $DIR/usr/local/bin

chmod +x $DIR/usr/local/bin/*

rm -rf $DIR/usr/share/*
rm -rf $DIR/var/cache/apt/*