#!/bin/sh

ROOT=" AUTHORS COPYING ChangeLog FAQ INSTALL Makefile  NEWS README "
FILES="cmd/ tools/kasudowrapper/ tools/libboot/ tools/boot tools/cookbook/install_scripts/ share/mysql/ tools/cookbook/install_scripts/ tools/cookbook/uninstall_scripts/ tools/cookbook/install_scripts/ tools/cookbook/conf/"

TOOLS="tools/kasudowrapper/kasudowrapper.sh tools/libboot"

MAN="man/man1"

#Making mans pages before
make -C man/src/
tar czf kadeploy-2.1.5.tgz $ROOT $FILES $TOOLS $MAN --exclude=*.svn*
