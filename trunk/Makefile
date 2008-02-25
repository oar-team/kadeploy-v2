#!/usr/bin/make

#######################################
#
# Installation Makefile for Kadeploy
# Grid'5000 project 
#
#######################################


#=========
# Version
#=========

MAJOR = 2
MINOR = 1
SUBMINOR = 7
KADEPLOY_VERSION = $(MAJOR).$(MINOR).$(SUBMINOR)


#=============================================
# Modify to adapt at your local environment
#=============================================

# Distribution : for Perl library installation path
DISTRIB = debian4

# Kadeploy directories installation layout
PREFIX=/usr/local

# Kadeploy main installation directory
KADEPLOYHOMEDIR=$(PREFIX)/kadeploy-$(KADEPLOY_VERSION)

# Kadeploy main configuration directory
KADEPLOYCONFDIR=/etc/kadeploy-$(KADEPLOY_VERSION)

# Kadeploy system user
DEPLOYUSER=deploy
DEPLOYGROUP=deploy


#=====================
# DO NOT modify below 
#=====================

SHELL=/bin/bash

ifeq ($(DISTRIB), debian4)
PERLDIR=/usr/share/perl/5.8/
else ifeq ($(DISTRIB), fedora4)
PERLDIR=/usr/lib/perl5/5.8.6/
else
PERLDIR=/usr/share/perl/5.8/
$(info $(DISTRIB) : using default Perl path : $(PERLDIR) )
endif

MANDIR=$(PREFIX)/man
INFODIR=$(PREFIX)/info
BINDIR=$(PREFIX)/bin
SBINDIR=$(PREFIX)/sbin

KABINDIR=$(KADEPLOYHOMEDIR)/bin
KASBINDIR=$(KADEPLOYHOMEDIR)/sbin
KAADDONSDIR=$(KADEPLOYHOMEDIR)/addons
KAPERLDIR=$(KADEPLOYHOMEDIR)/share/perl/5.8
KADBDIR=$(KADEPLOYHOMEDIR)/db

KADEPLOY_BINFILES=kaconsole kaenvironments karecordenv kadeploy kareboot karemote kaaddkeys kadatabase
KADEPLOY_SBINFILES=kaadduser kadeluser kanodes
KADEPLOY_MANPAGES=kaadduser.1 kaaddkeys.1 kaconsole.1 kadeluser.1 kaenvironments.1 karecordenv.1 kadeploy.1 kareboot.1 deploy.conf.1 karemote.1 deploy_cmd.conf.1


#==================
# Archive creation
#==================

MANPAGES_SRC=docs/man/src/
MANPAGES=docs/man/man1/
DOCUMENTATION_SRC=docs/texi/
DOCUMENTATION=$(DOCUMENTATION_SRC)/documentation/
SCRIPTS=scripts/
ADDONS=addons/
TOOLS=tools/

EXCLUDED=--exclude=.svn
KADEPLOY_ARC=kadeploy-$(KADEPLOY_VERSION).tar



.PHONY: all usage root_check user_and_group_deploy_check \
links_install directories files_install kadeploy_install sudo_install man_install \
kadeploy_uninstall files_uninstall \
archive scripts_arc addons_arc tools_arc manpages_arc manpages documentation documentation_arc
	

#################
#
# Default action
#
#################

all: usage


########################################
# 
# Installation or uninstallation checks
# 
########################################

#Check if you execute installation with root privileges
root_check:
	@echo "root check ..."
	@[ `whoami` = 'root' ] || ( echo "Warning: root-privileges are required to install some files !" ; exit 1 )

#Add the "proxy" user/group to /etc/passwd if needed.
user_and_group_deploy_check: 
	@# grep -q "^deploy:" /etc/passwd || adduser --system --home /home/deploy --group deploy
	@# grep -q "^deploy:" /etc/passwd || useradd -d /home/deploy/ -r deploy
	@echo "User and group check ..."
	@( grep -q "^deploy:" /etc/passwd && grep -q "^deploy:" /etc/group ) || \
	  adduser --system --group --no-create-home --home $(KADEPLOYHOMEDIR) $(DEPLOYUSER)

links_install:
	@echo "Making links to sudowrapper ..."
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/kaconsole
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/kaenvironments
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/karecordenv
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/kadeploy
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/kareboot
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/karemote
	@ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR)/kadatabase
# ln -s $(KABINDIR)/kasudowrapper.sh $(BINDIR/kaaddkeys

	@ln -s $(KABINDIR)/kasudowrapper.sh $(SBINDIR)/kaadduser
	@ln -s $(KABINDIR)/kasudowrapper.sh $(SBINDIR)/kadeluser
	@ln -s $(KABINDIR)/kasudowrapper.sh $(SBINDIR)/kanodes

directories:
	@echo "Making directories ..."
	@mkdir -p $(KADEPLOYHOMEDIR)/db
	@mkdir -p $(KADEPLOYHOMEDIR)/grub
	@mkdir -p $(KABINDIR)	
	@mkdir -p $(KASBINDIR)
	@mkdir -p $(KAPERLDIR)/libkadeploy2
	@mkdir -p -m 700 $(KADEPLOYCONFDIR)
	@mkdir -p $(BINDIR)
	@mkdir -p $(SBINDIR)

files_install:
	@echo "Copying files ..."
	@install -m 600 conf/deploy.conf $(KADEPLOYCONFDIR)/
	@install -m 600 conf/deploy_cmd.conf $(KADEPLOYCONFDIR)/
	@install -m 600 conf/clusternodes.conf $(KADEPLOYCONFDIR)/
	@install -m 600 conf/clusterpartition.conf $(KADEPLOYCONFDIR)/
	@chown -R $(DEPLOYUSER):root $(KADEPLOYCONFDIR)
	@chmod -R 700 $(KADEPLOYCONFDIR)

	@install -m 755 bin/kaconsole $(KABINDIR)/
	@install -m 755 bin/kaenvironments  $(KABINDIR)/
	@install -m 755 bin/karecordenv $(KABINDIR)/
	@install -m 755 bin/kadeploy $(KABINDIR)/
	@install -m 755 bin/kareboot $(KABINDIR)/
	@install -m 755 bin/karemote $(KABINDIR)/
	@install -m 755 bin/kadatabase $(KABINDIR)/
	@install -m 755 bin/kasudowrapper.sh $(KABINDIR)/	
	@install -m 755 bin/kaaddkeys $(BINDIR)/

	@install -m 755 sbin/kaadduser $(KASBINDIR)/
	@install -m 755 sbin/kadeluser $(KASBINDIR)/
	@install -m 755 sbin/kastats $(KASBINDIR)/	
	@install -m 755 sbin/kanodes $(KASBINDIR)/
	@install -m 755 sbin/setup_pxe.pl $(KASBINDIR)/

# Perl modules 
	@install -m 755 libkadeploy2/* $(KAPERLDIR)/libkadeploy2/
	@ln -s $(KAPERLDIR)/libkadeploy2 $(PERLDIR)/libkadeploy2

# database scripts
	@install -m 755 scripts/install/kadeploy_db_init.pl $(KADBDIR)
	@install -m 755 scripts/install/kadeploy_conflib.pm $(KADBDIR)
	@install -m 755 scripts/sql/*.sql $(KADBDIR)
	
# GRUB files
	@install -m 755 addons/grub/* $(KADEPLOYHOMEDIR)/grub
	
#Kadeploy installation in main directory
kadeploy_install: root_check user_and_group_deploy_check directories files_install links_install sudo_install man_install
	@chown -R deploy: $(KADEPLOYCONFDIR) 		  

#Sudo installation : modification of /etc/sudoers
sudo_install:
	@[ -e /etc/sudoers ] || ( echo "Error: No /etc/sudoers file. Is sudo installed ?" ; exit 1 )
	@sed -i "s%DEPLOYDIR=.*%DEPLOYDIR=$(KADEPLOYHOMEDIR)%" $(KABINDIR)/kasudowrapper.sh
	@scripts/uninstall/sudoers_uninstall.pl $(KADEPLOYHOMEDIR)
	@scripts/install/sudoers_install.pl $(KADEPLOYHOMEDIR)


#Install and creation of mans
man_install:
	@mkdir -p $(MANDIR)/man1
	@install -m 755 $(MANPAGES)/* $(MANDIR)/man1/

#Install info documentation
#info_install:
#	@mkdir -p $(INFODIR)
#	@install -m 755 docsDocumentation/info/*  $(INFODIR)/

###########################
#
# Kadeploy un-installation
# 
###########################

#Remove Installation of Kadeploy 
files_uninstall :
	@echo "Removing system-wide installed files ..."
	@cd $(BINDIR) && rm -f $(KADEPLOY_BINFILES)
	@cd $(SBINDIR) && rm -f $(KADEPLOY_SBINFILES)
	@cd $(MANDIR) && rm -f $(KADEPLOY_MANPAGES)
     
	@echo "Uninstalling sudowrapper ..."
	@scripts/uninstall/sudoers_uninstall.pl $(KADEPLOYHOMEDIR)
	
	@echo "Removing deploy user and group ..."
	@( grep -q $(DEPLOYUSER) /etc/passwd && userdel $(DEPLOYUSER) ) \
	|| echo "user $(DEPLOYUSER) already removed."
	@( grep -q $(DEPLOYGROUP) /etc/group && groupdel $(DEPLOYGROUP) ) \
	|| echo "group $(DEPLOYGROUP) already removed."
	
	@echo "Deleting Kadeploy homedir ..."
	@rm -rf $(KADEPLOYHOMEDIR)/
	@echo "Deleting Kadeploy confdir ..."
	@rm -rf $(KADEPLOYCONFDIR)/


kadeploy_uninstall : root_check files_uninstall


#############################
# 
# Archive creation (tarball)
# 
#############################

archive: manpages_arc documentation_arc scripts_arc addons_arc tools_arc
	@echo "Archiving Kadeploy main files ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) bin/
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) sbin/
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) libkadeploy2/
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) conf/
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) AUTHORS
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) COPYING
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) README
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) Makefile
	@echo "Compressing archive ..."
	@gzip $(KADEPLOY_ARC)
	@echo "Done."

scripts_arc:
	@echo "Archiving scripts ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(SCRIPTS)
	
addons_arc:
	@echo "Archiving addons ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(ADDONS)
	
tools_arc:
	@echo "Archiving tools ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(TOOLS)
	
manpages_arc: manpages
	@echo "Archiving Manpages ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(MANPAGES)
	
manpages:
	@make -C $(MANPAGES_SRC) 2>&1 >/dev/null
	
documentation_arc: documentation
	@echo "Archiving documentation ..."
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(DOCUMENTATION)
	@tar $(EXCLUDED) -C $(DOCUMENTATION_SRC) -rf $(KADEPLOY_ARC) INSTALL
	@tar $(EXCLUDED) -C $(DOCUMENTATION_SRC) -rf $(KADEPLOY_ARC) changelog.txt
	
documentation:
	@( cd $(DOCUMENTATION_SRC) && $(MAKE) 2>&1 >/dev/null )


################
#
# Usage of make
# 
################

usage:
	@echo -e "\n\t**************************************"
	@echo -e "\t*** Installation of Kadeploy-$(KADEPLOY_VERSION) ***"
	@echo -e "\t**************************************"
	@echo -e "\n\tUsage: make [ OPTIONS=<...> ] MODULES"
	@echo -e "\t\twhere MODULES := { kadeploy_install | kadeploy_uninstall | archive }"
	@echo -e "\t\tand   OPTIONS := { PREFIX | KADEPLOYHOMEDIR | KADEPLOYCONFDIR } \n"
