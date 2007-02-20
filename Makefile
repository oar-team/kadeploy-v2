#!/usr/bin/make

SHELL=/bin/sh
PREFIX=/usr/local

KADEPLOYHOMEDIR=$(PREFIX)/kadeploy
MANDIR=$(PREFIX)/man
BINDIR=$(KADEPLOYHOMEDIR)/bin
SBINDIR=$(KADEPLOYHOMEDIR)/sbin
LIBDIR=$(KADEPLOYHOMEDIR)/lib
PERLDIR=$(KADEPLOYHOMEDIR)/share/perl/5.8/libkadeploy2

tftp_repository=/tftpboot/PXEClient/
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

	@grep -q "^deploy:" /etc/passwd || adduser --system --home /home/deploy --group deploy


#Links Installation in /usr/local/bin
links_install:
#	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaaddkeys
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaconsole
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaenvironments
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/karecordenv
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kadeploy
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kareboot
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/karemote
	
	
#Kadeploy installation in main directory
kadeploy_install:

	mkdir -p $(KADEPLOYHOMEDIR)
	mkdir -p $(BINDIR)	
	mkdir -p $(SBINDIR)
	mkdir -p $(LIBDIR)/grub
	mkdir -p $(LIBDIR)/deployment_kernel/x86
	mkdir -p $(LIBDIR)/deployment_kernel/x86_64
	mkdir -p $(PERLDIR)
	
	install -m 755 cmd/kaconsole $(BINDIR)/
	install -m 755 cmd/kaenvironments  $(BINDIR)/
	install -m 755 cmd/karecordenv $(BINDIR)/
	install -m 755 cmd/kadeploy $(BINDIR)/
	install -m 755 cmd/kareboot $(BINDIR)/
	install -m 755 cmd/karemote $(BINDIR)/
	install -m 755 tools/kasudowrapper/kasudowrapper.sh $(BINDIR)/	
	
	install -m 755 tools/libboot/deployment_kernel/x86/* $(LIBDIR)/deployment_kernel/x86/
	install -m 755 tools/libboot/deployment_kernel/x86_64/* $(LIBDIR)/deployment_kernel/x86_64/
	install -m 755 tools/libboot/grub/* $(LIBDIR)/grub/

	install -m 755 cmd/kaadduser $(SBINDIR)/
	install -m 755 cmd/kadeluser $(SBINDIR)/
	install -m 755 cmd/kastats $(SBINDIR)/	
	install -m 755 tools/boot/setup_pxe.pl $(SBINDIR)/
	install -m 755 cmd/kanodes $(SBINDIR)/

	install -m 755 cmd/libkadeploy2/* $(PERLDIR)/ 


#Install and creation of mans
install_man:
	make -C man/src/
	install -m 755 man/man1/* $(MANDIR)/man1/

#Re 
remove_installation:
	rm -rf $(KADEPLOYHOMEDIR)
	rm -f $(PREFIX)/bin/kaaddkeys
	rm -f $(PREFIX)/bin/kaconsole
	rm -f $(PREFIX)/bin/kaenvironments
	rm -f $(PREFIX)/bin/karecordenv
	rm -f $(PREFIX)/bin/kadeploy
	rm -f $(PREFIX)/bin/kareboot
	rm -f $(PREFIX)/bin/karemote

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
	
	make clean -C man/src
	
	userdel -r $(DEPLOYUSER)
	groupdel $(DEPLOYGROUP)

#Main part of the installation
install: root_check checks_user_and_group_deploy kadeploy_install links_install install_man

#Installation of ftp part
tftp_install:
	@echo "Installation of tftp"
	
	mkdir -p $(tftp_repository)$(pxe_rep)
	mkdir -p $(tftp_repository)$(tftp_relative_path)

	chown $(DEPLOYUSER)  $(tftp_repository)$(tftp_relative_path)
	chown $(DEPLOYUSER)  $(tftp_repository)$(pxe_rep)

	install -m 755 tools/libboot/pxelinux/pxelinux.0 $(tftp_repository) 
	install -m 755 tools/libboot/pxelinux/memdisk $(tftp_repository)
	install -m 755 $(LIBDIR)/deployment_kernel/$(ARCH)/* $(tftp_repository)$(tftp_relative_path)
	

#Installation cleaning
clean: root_check remove_installation


#Usage of make
usage:
	@echo "Installation of Kadeploy 2.1.5."
	@echo "Usage: make [ OPTIONS=<...> ] MODULES"
	@echo "Where MODULES := { install | tftp_install | clean}"
	@echo "OPTIONS :={KADEPLOYHOMEDIR | ARCH | tftp_repository | pxe_rep | image_grub}"
