#!/bin/sh

# to retrieve a clean source tree (no .svn ...):
# svn export svn://scm.gforge.inria.fr/svn/kadeploy/tags/2.1.5

ROOT=" AUTHORS COPYING ChangeLog INSTALL Makefile  NEWS README "
FILES="cmd/ tools/kasudowrapper/ tools/libboot/ tools/boot tools/cookbook/install_scripts/ share/mysql/ tools/cookbook/install_scripts/ tools/cookbook/uninstall_scripts/ tools/cookbook/install_scripts/ tools/cookbook/conf/"

TOOLS="tools/kasudowrapper/kasudowrapper.sh tools/libboot"

MAN="man/man1"

#Making mans pages before
make -C man/src/
#Setting execution rights to installation scripts
chmod 755 tools/cookbook/install_scripts/*
#Create archive
tar czf kadeploy-2.1.5.tgz $ROOT $FILES $TOOLS $MAN
