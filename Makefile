#!/usr/bin/make
#
#This program is derived from oar project's sources (http//:oar.imag.fr)
#

SHELL=/bin/sh
PREFIX=/usr/local

KADEPLOYHOMEDIR=$(PREFIX)/kadeploy
MANDIR=$(PREFIX)/man
INFODIR=$(PREFIX)/info
BINDIR=$(KADEPLOYHOMEDIR)/bin
SBINDIR=$(KADEPLOYHOMEDIR)/sbin
LIBDIR=$(KADEPLOYHOMEDIR)/lib
PERLDIR=$(KADEPLOYHOMEDIR)/share/perl/5.8/libkadeploy2
KADEPLOYCONFDIR=/etc/kadeploy

tftp_repository=/var/lib/tftpboot/PXEClient/
pxe_rep=pxelinux.cfg/
tftp_relative_path=images_grub
ARCH=x86_64

DEPLOYUSER=deploy
DEPLOYGROUP=deploy

all: usage

#Check if you execute installation with root privileges
root_check:
	@[ `whoami` = 'root' ] || ( echo "Warning: root-privileges are required to install some files !" ; exit 1 )

#Add the "proxy" user/group to /etc/passwd if needed.
checks_user_and_group_deploy: 

	@grep -q "^deploy:" /etc/passwd || useradd -d /home/deploy/ -r deploy 

#Links Installation in /usr/local/bin
links_install:
#Users_Tools
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaconsole
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaenvironments
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/karecordenv
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kadeploy
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kareboot
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/karemote

#Admin_Tools
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/sbin/kaadduser
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/sbin/kadeluser
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/sbin/kanodes

#Kadeploy installation in main directory
kadeploy_install:

	mkdir -p $(KADEPLOYHOMEDIR)
	mkdir -p $(BINDIR)	
	mkdir -p $(SBINDIR)
	mkdir -p $(LIBDIR)/grub
	mkdir -p $(LIBDIR)/deployment_kernel/x86
	mkdir -p $(LIBDIR)/deployment_kernel/x86_64
	mkdir -p $(PERLDIR)
	mkdir -p -m 700 $(KADEPLOYCONFDIR)

	install -m 600 tools/cookbook/conf/deploy.conf $(KADEPLOYCONFDIR)/
	install -m 600 tools/cookbook/conf/deploy_cmd.conf $(KADEPLOYCONFDIR)/
	install -m 600 tools/cookbook/conf/clusternodes.conf $(KADEPLOYCONFDIR)/
	install -m 600 tools/cookbook/conf/clusterpartition.conf $(KADEPLOYCONFDIR)/

	chown -R $(DEPLOYUSER) $(KADEPLOYCONFDIR)

	install -m 755 cmd/kaconsole $(BINDIR)/
	install -m 755 cmd/kaenvironments  $(BINDIR)/
	install -m 755 cmd/karecordenv $(BINDIR)/
	install -m 755 cmd/kadeploy $(BINDIR)/
	install -m 755 cmd/kareboot $(BINDIR)/
	install -m 755 cmd/karemote $(BINDIR)/
	install -m 755 tools/kasudowrapper/kasudowrapper.sh $(BINDIR)/	

	install -m 755 tools/addons/kaaddkeys $(PREFIX)/bin/kaaddkeys

	install -m 755 tools/libboot/deployment_kernel/x86/* $(LIBDIR)/deployment_kernel/x86/
	install -m 755 tools/libboot/deployment_kernel/x86_64/* $(LIBDIR)/deployment_kernel/x86_64/
	install -m 755 tools/libboot/grub/* $(LIBDIR)/grub/

	install -m 755 cmd/kaadduser $(SBINDIR)/
	install -m 755 cmd/kadeluser $(SBINDIR)/
	install -m 755 cmd/kastats $(SBINDIR)/	
	install -m 755 tools/boot/setup_pxe.pl $(SBINDIR)/
	install -m 755 cmd/kanodes $(SBINDIR)/

	install -m 755 cmd/libkadeploy2/* $(PERLDIR)/ 

	sed -i "s%kadeploy2_directory[\ |\t]*=.*%kadeploy2_directory = $(KADEPLOYHOMEDIR)%" $(KADEPLOYCONFDIR)/deploy.conf
	sed -i "s%DEPLOYDIR=.*%DEPLOYDIR=$(KADEPLOYHOMEDIR)%" $(BINDIR)/kasudowrapper.sh

#Database scripts installation
db_install:
	mkdir -p $(KADEPLOYHOMEDIR)/
	install -m 755 tools/cookbook/install_scripts/kadeploy_db_init.pl $(KADEPLOYHOMEDIR)/
	install -m 644 share/mysql/create_table_deploy.sql $(KADEPLOYHOMEDIR)/
	install -m 644 tools/cookbook/install_scripts/kadeploy_conflib.pm $(KADEPLOYHOMEDIR)/


#Sudo installation part. Modification of /etc/sudoers
sudo_install:
	@[ -e /etc/sudoers ] || ( echo "Error: No /etc/sudoers file. Is sudo installed ?" ; exit 1 )
	tools/cookbook/uninstall_scripts/sudoers_uninstall.pl $(KADEPLOYHOMEDIR)
	tools/cookbook/install_scripts/sudoers_install.pl $(KADEPLOYHOMEDIR)


#Install and creation of mans
install_man:
	mkdir -p $(MANDIR)/man1
	install -m 755 man/man1/* $(MANDIR)/man1/

#Install info documentation
install_info:
	mkdir -p $(INFODIR)
	install -m 755 Documentation/info/*  $(INFODIR)/

#Installation of ftp part
tftp_install:
	@echo "Installation of tftp"

	mkdir -p /$(tftp_repository)/$(pxe_rep)
	mkdir -p /$(tftp_repository)/$(tftp_relative_path)

	chown $(DEPLOYUSER)  $(tftp_repository)$(tftp_relative_path)
	chown $(DEPLOYUSER)  $(tftp_repository)$(pxe_rep)

	install -m 644 tools/libboot/pxelinux/pxelinux.0 $(tftp_repository) 
	install -m 644 tools/libboot/pxelinux/memdisk $(tftp_repository)$(tftp_relative_path)
	install -m 644 $(LIBDIR)/deployment_kernel/$(ARCH)/* $(tftp_repository)$(tftp_relative_path)

	sed -i "s%^tftp_repository[\ |\t]*=.*%tftp_repository = $(tftp_repository)%" $(KADEPLOYCONFDIR)/deploy.conf
	sed -i "s%^pxe-rep[\ |\t]*=.*%pxe_rep = $(pxe_rep)%" $(KADEPLOYCONFDIR)/deploy.conf
	sed -i "s%^tftp_relative_path[\ |\t]*=.*%tftp_relative_path = $(tftp_relative_path)%" $(KADEPLOYCONFDIR)/deploy.conf	


#Remove Installation of Kadeploy 
remove_installation:
	rm -rf $(KADEPLOYHOMEDIR)
	rm -f $(PREFIX)/bin/kaaddkeys
	rm -f $(PREFIX)/bin/kaconsole
	rm -f $(PREFIX)/bin/kaenvironments
	rm -f $(PREFIX)/bin/karecordenv
	rm -f $(PREFIX)/bin/kadeploy
	rm -f $(PREFIX)/bin/kareboot
	rm -f $(PREFIX)/bin/karemote

	rm -f $(PREFIX)/sbin/kaadduser
	rm -f $(PREFIX)/sbin/kadeluser
	rm -f $(PREFIX)/sbin/kanodes	

	rm -f $(MANDIR)/kaadduser.1
	rm -f $(MANDIR)/kaaddkeys.1
	rm -f $(MANDIR)/kaconsole.1
	rm -f $(MANDIR)/kadeluser.1
	rm -f $(MANDIR)/kaenvironments.1
	rm -f $(MANDIR)/karecordenv.1
	rm -f $(MANDIR)/kadeploy.1
	rm -f $(MANDIR)/kareboot.1
	rm -f $(MANDIR)/deploy.conf.1
	rm -f $(MANDIR)/karemote.1
	rm -f $(MANDIR)/deploy_cmd.conf.1

	rm -f $(INFODIR)/kadeploy-info.info.gz

	rm -rf $(KADEPLOYCONFDIR)/

	tools/cookbook/uninstall_scripts/sudoers_uninstall.pl $(KADEPLOYHOMEDIR)

	userdel -r $(DEPLOYUSER)

#Database scripts installation
db_uninstall:
	rm $(KADEPLOYHOMEDIR)/kadeploy_db_init.pl 
	rm $(KADEPLOYHOMEDIR)/create_table_deploy.sql 
	rm $(KADEPLOYHOMEDIR)/kadeploy_conflib.pm 


#Main part of the installation
install: root_check checks_user_and_group_deploy kadeploy_install links_install sudo_install install_man install_info

#Installation cleaning
uninstall: root_check remove_installation


#Usage of make
usage:
	@echo "Installation of Kadeploy 2.1.5."
	@echo "Usage: make [ OPTIONS=<...> ] MODULES"
	@echo "Where MODULES := { install | db_install | tftp_install | uninstall | db_uninstall}"
	@echo "OPTIONS :={KADEPLOYHOMEDIR | ARCH | tftp_repository | pxe_rep | image_grub}"
