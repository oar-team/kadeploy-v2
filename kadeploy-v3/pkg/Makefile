KADEPLOY_ROOT=..
DEPLOY_USER=deploy
DEB_ROOT=kadeploy
SRC=$(KADEPLOY_ROOT)/src
BIN=$(KADEPLOY_ROOT)/bin
SBIN=$(KADEPLOY_ROOT)/sbin
CONF=$(KADEPLOY_ROOT)/conf
ADDONS=$(KADEPLOY_ROOT)/addons
TEST=$(KADEPLOY_ROOT)/test
default: package

install_src:
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/lib/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src/lib
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/contrib/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src/contrib
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/cmdctrl/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib/cmdctrl
	@install -o $(DEPLOY_USER) -m 644 $(SRC)/contrib/cmdctrl-0.3/lib/cmdctrl/commands/*.rb $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib/cmdctrl/commands

install_conf:
	@install -o $(DEPLOY_USER) -m 600 $(CONF)/conf $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 600 $(CONF)/specific_conf* $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 600 $(CONF)/nodes $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 600 $(CONF)/cmd $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 600 $(CONF)/fdisk* $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 755 $(CONF)/load_kadeploy_env $(DEB_ROOT)/etc/kadeploy3
	@install -o $(DEPLOY_USER) -m 644 $(CONF)/client_conf $(DEB_ROOT)/etc/kadeploy3

install_bin:
	@install -o $(DEPLOY_USER) -m 755 $(BIN)/* $(DEB_ROOT)/usr/bin

install_sbin:
	@install -o $(DEPLOY_USER) -m 700 $(SBIN)/* $(DEB_ROOT)/usr/sbin

install_debootstrap:
	@install -o $(DEPLOY_USER) -m 700 $(ADDONS)/deploy_env_generation/debootstrap/linuxrc $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap
	@install -o $(DEPLOY_USER) -m 700 $(ADDONS)/deploy_env_generation/debootstrap/mkdev $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap
	@install -o $(DEPLOY_USER) -m 700 $(ADDONS)/deploy_env_generation/debootstrap/make_debootstrap.sh $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap
	@install -o $(DEPLOY_USER) -m 700 $(ADDONS)/deploy_env_generation/debootstrap/make_kernel.sh $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap
	@install -o $(DEPLOY_USER) -m 700 $(ADDONS)/deploy_env_generation/debootstrap/scripts/* $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap/scripts

install_test:
	@install -o $(DEPLOY_USER) -m 644 $(TEST)/envfile.dsc $(DEB_ROOT)/usr/local/kadeploy3/test
	@install -o $(DEPLOY_USER) -m 700 $(TEST)/blackbox_tests.rb $(DEB_ROOT)/usr/local/kadeploy3/test

install_rc_script:
	@install -m 644 $(ADDONS)/rc/kadeploy_server $(DEB_ROOT)/etc/init.d

install_ssh_key:
	@install -o $(DEPLOY_USER) -m 400 $(ADDONS)/ssh/id_deploy $(DEB_ROOT)/.keys

install_files: install_src install_conf install_bin install_sbin install_debootstrap install_test install_rc_script install_ssh_key

tree:
	@mkdir -p $(DEB_ROOT)/usr/bin
	@mkdir -p $(DEB_ROOT)/usr/sbin
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/lib
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/addons
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap/scripts
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/addons/deploy_env_generation/debootstrap/ssh
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/db
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/test
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/contrib
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib/cmdctrl
	@mkdir -p $(DEB_ROOT)/usr/local/kadeploy3/src/contrib/cmdctrl-0.3/lib/cmdctrl/commands
	@mkdir -p $(DEB_ROOT)/etc/kadeploy3
	@mkdir -p $(DEB_ROOT)/etc/init.d
	@mkdir -p $(DEB_ROOT)/.keys

package: tree install_files
	@dpkg-deb --build $(DEB_ROOT) kadeploy-v3.deb

clean:
	@rm -rf $(DEB_ROOT)/usr $(DEB_ROOT)/etc  $(DEB_ROOT)/.keys
