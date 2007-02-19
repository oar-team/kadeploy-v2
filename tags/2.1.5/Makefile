#!/usr/bin/make

SHELL=/bin/sh
PREFIX=/usr/local

KADEPLOYHOMEDIR=$(PREFIX)/kadeploy
MANDIR=$(PREFIX)/man
BINDIR=$(KADEPLOYHOMEDIR)/bin
SBINDIR=$(KADEPLOYHOMEDIR)/sbin
LIBDIR=$(KADEPLOYHOMEDIR)/lib
PERLDIR=$(KADEPPLOYHOMEDIR)share/perl/5.8/libkadeploy2

DEPLOYUSER=deploy
DEPLOYGROUP=deploy

all: usage

#Check if you execute installation with root privileges
root_check:
	@[ `whoami` = 'root' ] || ( echo "Warning: root-privileges are required to install some files !" ; exit -1 )

#Add the "proxy" user/group to /etc/passwd if needed.
checks_user_and_group_deploy: 

	@grep -q "^deploy:" /etc/passwd || adduser --system --home /home/deploy --group deploy


#Links Installation in /usr/local/bin
links_install:
	ln -s $(BINDIR)/kasudowrapper.sh $(PREFIX)/bin/kaaddkeys
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
	install -m 755 man/man1/ $(MANDIR)
 
remove_installation:
	rm -rf $(KADEPLOYHOMEDIR)
	rm $(PREFIX)/bin/kaaddkeys
	rm $(PREFIX)/bin/kaconsole
	rm $(PREFIX)/bin/kaenvironments
	rm $(PREFIX)/bin/karecordenv
	rm $(PREFIX)/bin/kadeploy
	rm $(PREFIX)/bin/kareboot
	rm $(PREFIX)/bin/karemote

	rm $(MANDIR)/kaaddkeys.1
	rm $(MANDIR)/kaconsole.1
	rm $(MANDIR)/kaenvironments.1
	rm $(MANDIR)/karecordenv.1
	rm $(MANDIR)/kadeploy.1
	rm $(MANDIR)/kareboot.1
	rm $(MANDIR)/deploy.conf.1
	rm $(MANDIR)/karemote.1
	rm $(MANDIR)/deploy_cmd.conf.1

	userdel -r $(DEPLOYUSER)
	groupdel $(DEPLOYGROUP)

#Main part of the installation
install: root_check checks_user_and_group_deploy kadeploy_install links_install

#Installation cleaning
clean: root_check remove_installation


#Usage of make
usage:
	@echo "Installation of Kadeploy 2.1.5. To install Kadeploy type make install. To uninstall Kadeploy type make clean"
