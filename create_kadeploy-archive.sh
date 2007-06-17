#!/bin/sh

# to retrieve a clean source tree (no .svn ...):
# svn export svn://scm.gforge.inria.fr/svn/kadeploy/<branch>

ROOT=" AUTHORS COPYING changelog.txt INSTALL Makefile README "
FILES="cmd/ tools/kasudowrapper/ tools/libboot/ tools/boot tools/cookbook/install_scripts/ share/mysql/ tools/cookbook/install_scripts/ tools/cookbook/uninstall_scripts/ tools/cookbook/install_scripts/ tools/cookbook/conf/"

TOOLS="tools/kasudowrapper/kasudowrapper.sh tools/libboot"

MAN="man/man1"

#Making mans pages before
make -C man/src/
#Making Documentation pages
make -C Documentation/texi/
# Copy files included on top of the archive
cp Documentation/texi/changelog.txt changelog.txt
cp Documentation/texi/INSTALL INSTALL
#Setting execution rights to installation scripts
chmod 755 tools/cookbook/install_scripts/*
#Create archive
tar czf kadeploy-2.1.5.tgz $ROOT $FILES $TOOLS $MAN
