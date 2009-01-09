#!/bin/bash

DIR=debootstrap
SCRIPTS_DIR=scripts

DEBOOTSTRAP="/usr/sbin/debootstrap"
DEBOOTSTRAP_INCLUDE_PACKAGES=dhcpcd,openssh-client,openssh-server,kexec-tools,bzip2,taktuk,grub-pc
DEBOOTSTRAP_EXCLUDE_PACKAGE=vim-common,vim-tiny,traceroute,manpages,man-db,adduser,cron,logrotate,laptop-detect,tasksel,tasksel-data,dhcp3-client,dhcp3-common,wget


mkdir -p $DIR

$DEBOOTSTRAP --include=$DEBOOTSTRAP_INCLUDE_PACKAGES --exclude=$DEBOOTSTRAP_EXCLUDE_PACKAGE lenny $DIR

(cd $DIR/bin ; ln -s bash ash)

echo "127.0.0.1       localhost" > $DIR/etc/hosts

echo "localhost" >  $DIR/etc/hostname

cat >> $DIR/root/.bashrc <<EOF
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin
export LC_ALL="C"
EOF

mkdir -p $DIR/root/.ssh
cp ssh/* $DIR/root/.ssh
cat ssh/id_deploy.pub > $DIR/root/.ssh/authorized_keys

cat > $DIR/etc/nsswitch.conf <<EOF
passwd:     files
group:      files

hosts:      files dns

ethers:     files
etmasks:   files
networks:   files
protocols:  files
rpc:        files
services:   files

netgroup:   nisplus

publickey:  nisplus

automount:  files
aliases:    files nisplus
EOF


rm $DIR/sbin/init

cp init $DIR/sbin/init
cp mkdev $DIR/dev

cp $SCRIPTS_DIR/* $DIR/usr/local/bin

chmod +x $DIR/usr/local/bin/*

mkdir $DIR/mnt/dest
mkdir $DIR/rambin
mkdir $DIR/mnt/tmp

#for d in `find $DIR/usr/share -mindepth 1 -maxdepth 1|grep -v perl`
#do
#    rm -rf $d
#done
rm -rf $DIR/var/cache/apt/*
