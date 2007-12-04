PKG_NAME		= kadeploy
PKG_VERSION		= 2.1.7
PKG_ARCH		= all

ROOT_DEB       		= ./debian/kadeploy
DOCUMENTATION_DEB	= $(ROOT_DEB)/usr/share/doc/kadeploy
MANPAGES_DEB		= $(ROOT_DEB)/usr/local/share/man/man1
INFO_DEB		= $(ROOT_DEB)/usr/share/info
BIN_DEB			= $(ROOT_DEB)/usr/local/kadeploy/bin
SBIN_DEB		= $(ROOT_DEB)/usr/local/kadeploy/sbin
LOCALBIN_DEB		= $(ROOT_DEB)/usr/local/bin
LOCALSBIN_DEB		= $(ROOT_DEB)/usr/local/sbin
PERLLIB_DEB		= $(ROOT_DEB)/usr/local/kadeploy/lib/perl/5.8/libkadeploy2
KERNEL_DEB		= $(ROOT_DEB)/usr/local/kadeploy/share/kernel
GRUB_DEB		= $(ROOT_DEB)/usr/local/kadeploy/share/grub
TOOLS_DEB		= $(ROOT_DEB)/usr/local/kadeploy/tools

ROOT_ARCHIVE		= ..
DOCUMENTATION_SRC	= $(ROOT_ARCHIVE)/Documentation/texi
MANPAGES_SRC		= $(ROOT_ARCHIVE)/man/src
BIN_SRC			= $(ROOT_ARCHIVE)/cmd
PERLLIB_SRC		= $(BIN_SRC)/libkadeploy2
SUDOWRAPPER_SRC		= $(ROOT_ARCHIVE)/tools/kasudowrapper
ADDONS_SRC		= $(ROOT_ARCHIVE)/tools/addons
BOOT_SRC		= $(ROOT_ARCHIVE)/tools/boot
KERNEL_SRC		= $(ROOT_ARCHIVE)/tools/libboot/deployment_kernel
GRUB_SRC		= $(ROOT_ARCHIVE)/tools/libboot/grub
CONF_SRC		= $(ROOT_ARCHIVE)/tools/cookbook/conf
MYSQL_SRC		= $(ROOT_ARCHIVE)/share/mysql
INSTALLSCRIPTS_SRC	= $(ROOT_ARCHIVE)/tools/cookbook/install_scripts
UNINSTALLSCRIPTS_SRC	= $(ROOT_ARCHIVE)/tools/cookbook/uninstall_scripts

man_deb: man
	install -m 644 $(ROOT_ARCHIVE)/man/man1/* $(MANPAGES_DEB)/

doc_deb: doc
	install -m 644 $(DOCUMENTATION_SRC)/changelog.txt $(DOCUMENTATION_DEB)/
	install -m 644 $(DOCUMENTATION_SRC)/INSTALL $(DOCUMENTATION_DEB)/
	install -m 644 $(DOCUMENTATION_SRC)/documentation/kadeploy.info.tar.gz $(INFO_DEB)/
	install -m 644 $(CONF_SRC)/* $(DOCUMENTATION_DEB)/examples/
	 
man:
	make -C $(MANPAGES_SRC)
	 	 
doc:
	make -C $(DOCUMENTATION_SRC)
	   
install_sudowrapper_deb:
	install -m 755 $(SUDOWRAPPER_SRC)/kasudowrapper.sh $(BIN_DEB)/
	   
install_bin_deb:
	install -m 755 $(BIN_SRC)/kaconsole $(BIN_DEB)/
	install -m 755 $(BIN_SRC)/kaenvironments $(BIN_DEB)/
	install -m 755 $(BIN_SRC)/karecordenv $(BIN_DEB)/
	install -m 755 $(BIN_SRC)/kadeploy $(BIN_DEB)/
	install -m 755 $(BIN_SRC)/kareboot $(BIN_DEB)/
	install -m 755 $(BIN_SRC)/karemote $(BIN_DEB)/
	install -m 755 $(ADDONS_SRC)/kaaddkeys $(LOCALBIN_DEB)

install_sbin_deb:
	install -m 755 $(BIN_SRC)/kaadduser $(SBIN_DEB)/
	install -m 755 $(BIN_SRC)/kadeluser $(SBIN_DEB)/
	install -m 755 $(BIN_SRC)/kastats $(SBIN_DEB)/
	install -m 755 $(BIN_SRC)/kanodes $(SBIN_DEB)/
	install -m 755 $(BOOT_SRC)/setup_pxe.pl $(SBIN_DEB)/
		 

install_perllib_deb:
	install -m 755 $(PERLLIB_SRC)/* $(PERLLIB_DEB)/

install_kernel_deb:
	install -m 644 $(KERNEL_SRC)/x86/* $(KERNEL_DEB)/
	install -m 644 $(KERNEL_SRC)/x86_64/* $(KERNEL_DEB)/

install_grub_deb:
	install -m 644 $(GRUB_SRC)/* $(GRUB_DEB)/

install_scripts_deb:
	install -m 755 $(MYSQL_SRC)/*.sql $(TOOLS_DEB)/sql/
	install -m 755 $(INSTALLSCRIPTS_SRC)/kadeploy_db_init.pl $(TOOLS_DEB)/
	install -m 755 $(INSTALLSCRIPTS_SRC)/kadeploy_conflib.pm $(TOOLS_DEB)/
	install -m 755 $(INSTALLSCRIPTS_SRC)/sudoers_install.pl $(TOOLS_DEB)/
	install -m 755 $(UNINSTALLSCRIPTS_SRC)/sudoers_uninstall.pl $(TOOLS_DEB)/

make_links_deb: install_bin_deb install_sbin_deb install_sudowrapper_deb
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kaconsole)
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kaenvironments)
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh karecordenv)
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kadeploy)
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kareboot)
	(cd $(LOCALBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh karemote)
		
	(cd $(LOCALSBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kaadduser)
	(cd $(LOCALSBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kadeluser)
	(cd $(LOCALSBIN_DEB) && ln -s ../kadeploy/bin/kasudowrapper.sh kanodes)
	   	
tree_deb:
	mkdir -p $(ROOT_DEB)/etc/kadeploy/
	mkdir -p $(ROOT_DEB)/usr/local/bin/
	mkdir -p $(ROOT_DEB)/usr/local/sbin/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/sbin/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/lib/perl/5.8/libkadeploy2/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/bin/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/sbin/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/share/kernel/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/share/grub/
	mkdir -p $(ROOT_DEB)/usr/local/kadeploy/tools/sql/
	mkdir -p $(ROOT_DEB)/usr/local/share/man/man1/
	mkdir -p $(ROOT_DEB)/usr/share/doc/kadeploy/examples/
	mkdir -p $(ROOT_DEB)/usr/share/info/

install_kadeploy_deb: make_links_deb install_perllib_deb install_grub_deb install_kernel_deb doc_deb man_deb install_scripts_deb

deb: tree_deb install_kadeploy_deb
	dpkg-deb --build $(ROOT_DEB) $(PKG_NAME)_$(PKG_VERSION)_$(PKG_ARCH).deb

clean:
	rm -rf $(ROOT_DEB)/etc/ $(ROOT_DEB)/usr/
