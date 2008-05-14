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
KADEPLOY_VERSION = "-$(MAJOR).$(MINOR).$(SUBMINOR)"


#=============================================
# Modify to adapt at your local environment
#=============================================

# Distribution : for Perl library installation path
DISTRIB = debian4

# Kadeploy directories installation layout
PREFIX=/usr/local

# Kadeploy main installation directory
KADEPLOYHOMEDIR=$(PREFIX)/kadeploy$(KADEPLOY_VERSION)

# Kadeploy system user
DEPLOYUSER=deploy
DEPLOYGROUP=deploy


#=====================
# DO NOT modify below 
#=====================

SHELL=/bin/bash

$(info Use settings for distribution : $(DISTRIB) )
ifeq ($(DISTRIB), debian4)
PERLDIR=/usr/share/perl/5.8/
HTMLDOC=/usr/bin/htmldoc
XSLTPROC=/usr/bin/xsltproc
TLDPOPXSL=/usr/share/xml/docbook/stylesheet/ldp/html/tldp-one-page.xsl
else ifeq ($(DISTRIB), fedora4)
PERLDIR=/usr/lib/perl5/5.8.6/
HTMLDOC=/usr/bin/htmldoc
XSLTPROC=/usr/bin/xsltproc
TLDPOPXSL=/usr/share/xml/docbook/stylesheet/ldp/html/tldp-one-page.xsl
else
PERLDIR=/usr/share/perl/5.8/
$(info $(DISTRIB) : using default Perl path : $(PERLDIR) )
HTMLDOC=/usr/bin/htmldoc
$(info $(DISTRIB) : using default htmldoc path : $(HTMLDOC) )
XSLTPROC=/usr/bin/xsltproc
$(info $(DISTRIB) : using default xsltproc path : $(XSLTPROC) )
TLDPOPXSL=/usr/share/xml/docbook/stylesheet/ldp/html/tldp-one-page.xsl
$(info $(DISTRIB) : using default LPD XSL stylesheet : $(TLDPOPXSL) )
endif

# Kadeploy main configuration directory
CONFDIR=/etc/kadeploy
KADEPLOYCONFDIR=$(CONFDIR)$(KADEPLOY_VERSION)


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

GROUPS=/etc/group
USERS=/etc/passwd

#==================
# Archive creation
#==================

MANPAGES_SRC=docs/man/src/
MANPAGES=docs/man/man1/
DOCUMENTATION_SRC=docs/texi/
DOCUMENTATION=$(DOCUMENTATION_SRC)/documentation/
DOCBOOK=docs/docbook/
SCRIPTS=scripts/
ADDONS=addons/
TOOLS=tools/
PDF_DOCS=$(wildcard $(DOCBOOK)*.pdf)

EXCLUDED=--exclude='.svn' --exclude='*~'
KADEPLOY_ARC=kadeploy$(KADEPLOY_VERSION).tar



.PHONY: all usage installcheck root_check user_and_group_deploy_check \
links_install conflink_install installdirs files_install install sudo_install man_install \
uninstall files_uninstall \
dist scripts_arc addons_arc tools_arc manpages_arc manpages documentation documentation_arc 

	

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
installcheck: root_check user_and_group_deploy_check prefix_check

prefix_check:
	     @echo "Installation directory check ..."
	     @( test -d $(PREFIX) || ( mkdir -p $(PREFIX) && echo "$(PREFIX) created." ) )

root_check:
	@echo "root check ..."
	@[ `whoami` = 'root' ] || ( echo "Warning: root-privileges are required to install some files !" ; exit 1 )

#Add the "proxy" user/group to /etc/passwd if needed.
user_and_group_deploy_check: 
	@echo "User and group check ..."
	@( ( grep -q "^deploy:" $(GROUPS) && echo "deploy group already created." ) || \
		addgroup --quiet --system $(DEPLOYGROUP) )
	@( ( grep -q "^deploy:" $(USERS) && echo "deploy user already created." ) || \
		adduser --quiet --system --ingroup $(DEPLOYGROUP) --no-create-home --home $(KADEPLOYHOMEDIR) $(DEPLOYUSER) 2>&1 >/dev/null )

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

installdirs:
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
	@ln -sf $(KAPERLDIR)/libkadeploy2 $(PERLDIR)/libkadeploy2

# database scripts
	@install -m 755 scripts/install/kadeploy_db_init.pl $(KADBDIR)
	@install -m 755 scripts/install/kadeploy_conflib.pm $(KADBDIR)
	@install -m 755 scripts/sql/*.sql $(KADBDIR)
	
# GRUB files
	@install -m 755 addons/grub/* $(KADEPLOYHOMEDIR)/grub

conflink_install:
	@( [ -e $(CONFDIR) ] && ( mv $(CONFDIR) $(CONFDIR).old ) || echo No previously existing $(CONFDIR) found. )
	@( [ ! -e $(CONFDIR) ] && ( ln -s $(KADEPLOYCONFDIR) $(CONFDIR) ) || echo $(CONFDIR) already exists : not linked over. )
	
#Kadeploy installation in main directory
install: installcheck installdirs files_install links_install conflink_install sudo_install man_install
	@chown -R deploy: $(KADEPLOYCONFDIR) 		  

#Sudo installation : modification of /etc/sudoers
sudo_install:
	@[ -e /etc/sudoers ] || ( echo "Error: No /etc/sudoers file. Is sudo installed ?" && exit 1 )
	@sed -i "s%DEPLOYDIR=.*%DEPLOYDIR=$(KADEPLOYHOMEDIR)%" $(KABINDIR)/kasudowrapper.sh
	@sed -i "s%DEPLOYUSER=.*%DEPLOYUSER=$(DEPLOYUSER)%" $(KABINDIR)/kasudowrapper.sh
	@sed -i "s%PERL5LIBDEPLOY=.*%PERL5LIBDEPLOY=$(PERLDIR)%" $(KABINDIR)/kasudowrapper.sh
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
	@( [ -L $(CONFDIR) ] && ( rm -f $(CONFDIR) ) || echo No previously existing $(CONFDIR) found. )
	@( [ -d $(CONFDIR).old ] && ( mv $(CONFDIR).old $(CONFDIR) ) || echo No previously existing $(CONFDIR).old found. )

uninstall : root_check files_uninstall


#############################
# 
# Archive creation (tarball)
# 
#############################

dist: manpages_arc documentation_arc scripts_arc addons_arc tools_arc
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
	@tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(PDF_DOCS)
# @tar $(EXCLUDED) -C $(DOCUMENTATION_SRC) -rf $(KADEPLOY_ARC) INSTALL
# @tar $(EXCLUDED) -C $(DOCUMENTATION_SRC) -rf $(KADEPLOY_ARC) changelog.txt
# @tar $(EXCLUDED) -rf $(KADEPLOY_ARC) $(DOCUMENTATION)

check_htmldoc:
	@( test -f $(HTMLDOC) || ( echo "$(HTMLDOC) : command not found." && exit 1; ) )
	
check_xsltproc: 
	@( test -f $(XSLTPROC) || ( echo "$(XSLTPROC) : command not found." && exit 1; ) )

check_ldpxsl:
	@( test -f $(TLDPOPXSL) || ( echo "$(TLDPOPXSL) : command not found." && exit 1; ) )

documentation: check_htmldoc check_xsltproc check_ldpxsl
	@( cd $(DOCBOOK) && $(MAKE) 2>&1 >/dev/null )

################
#
# Usage of make
# 
################

usage:
	@echo -e "\n\t**************************************"
	@echo -e "\t*** Installation of Kadeploy$(KADEPLOY_VERSION) ***"
	@echo -e "\t**************************************"
	@echo -e "\n\tUsage: make [ OPTIONS=<...> ] MODULE"
	@echo -e "\t\t==> OPTIONS := { PREFIX | KADEPLOYHOMEDIR } "
	@echo -e "\t\t==> MODULE := { install | uninstall | dist }\n"
