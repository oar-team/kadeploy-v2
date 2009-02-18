KADEPLOY_ROOT=..
DEB_ROOT=kadeploy
SRC=$(KADEPLOY_ROOT)/src
BIN=$(KADEPLOY_ROOT)/bin
SBIN=$(KADEPLOY_ROOT)/sbin
CONF=$(KADEPLOY_ROOT)/conf
ADDONS=$(KADEPLOY_ROOT)/addons
TEST=$(KADEPLOY_ROOT)/test
default: package

install_src:
	install -m 644 $(SRC)/*.rb $(DEB_ROOT)/usr/local/kadeploy
	install -m 644 $(SRC)/lib/*.rb $(DEB_ROOT)/usr/local/kadeploy/lib
	install -m 644 $(SRC)/contrib/*.rb $(DEB_ROOT)/usr/local/kadeploy/contrib
	install -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/*.rb $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib
	install -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/cmdctrl/*.rb $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib/cmdctrl
	install -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/cmdctrl/commands/*.rb $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib/cmdctrl/commands

install_conf:
	install -m 600 $(CONF)/conf $(DEB_ROOT)/etc/kadeploy
	install -m 600 $(CONF)/specific_conf* $(DEB_ROOT)/etc/kadeploy
	install -m 600 $(CONF)/nodes $(DEB_ROOT)/etc/kadeploy
	install -m 600 $(CONF)/cmd $(DEB_ROOT)/etc/kadeploy	
	install -m 600 $(CONF)/fdisk* $(DEB_ROOT)/etc/kadeploy	
	install -m 755 $(CONF)/load_kadeploy_env $(DEB_ROOT)/etc/kadeploy
	install -m 644 $(CONF)/client_conf $(DEB_ROOT)/etc/kadeploy

install_bin:
	install -m 755 $(BIN)/* $(DEB_ROOT)/usr/bin

install_sbin:
	install -m 700 $(SBIN)/* $(DEB_ROOT)/usr/sbin

install_addons:
	install -m 700 $(ADDONS)/deploy_env_generation/debootstrap/linuxrc $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap
	install -m 700 $(ADDONS)/deploy_env_generation/debootstrap/mkdev $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap
	install -m 700 $(ADDONS)/deploy_env_generation/debootstrap/make_debootstrap.sh $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap
	install -m 700 $(ADDONS)/deploy_env_generation/debootstrap/make_kernel.sh $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap
	install -m 700 $(ADDONS)/deploy_env_generation/debootstrap/scripts/* $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap/scripts
	install -m 600 $(ADDONS)/deploy_env_generation/debootstrap/ssh/* $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap/ssh

install_test:
	install -m 644 $(TEST)/envfile.dsc $(DEB_ROOT)/usr/local/kadeploy/test
	install -m 700 $(TEST)/blackbox_tests.rb $(DEB_ROOT)/usr/local/kadeploy/test

install_files: install_src install_conf install_bin install_sbin install_addons install_test

tree:
	mkdir -p $(DEB_ROOT)/usr/bin
	mkdir -p $(DEB_ROOT)/usr/sbin
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/lib
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/addons
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap/scripts
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/addons/deploy_env_generation/debootstrap/ssh
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/db
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/test
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/contrib
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib/cmdctrl
	mkdir -p $(DEB_ROOT)/usr/local/kadeploy/contrib/cmdctrl-0.3/lib/cmdctrl/commands
	mkdir -p $(DEB_ROOT)/etc/kadeploy

package: tree install_files
	dpkg-deb --build $(DEB_ROOT) kadeploy-v3.deb

clean:
	rm -rf $(DEB_ROOT)/usr $(DEB_ROOT)/etc
