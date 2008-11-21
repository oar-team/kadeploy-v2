#!/bin/bash

DEBOOTSTRAP=/usr/sbin/debootstrap
NFSROOT=/nfsroot/diskless
TFTPROOT=/var/lib/tftpboot
KERNEL_HTTP_ROOT=http://kernel.org/pub/linux/kernel/v2.6
KERNEL_VERSION=2.6.27.7
CONFIG_FILE=/home/ejeanvoi/config
DEBOOTSTRAP_INCLUDE_PACKAGES=nfsbooted,dhcp3-client,procps,configure-debian,rsh-server,grub,kexec-tools,taktuk,bzip2
DEBOOTSTRAP_EXCLUDE_PACKAGE=openssh-client,vim-common,vim-tiny
TMP=/tmp
NAMESERVER=195.83.212.1

[ -x $DEBOOTSTRAP ] || ( echo "The $DEBOOTSTRAP command cannot be reached" ; exit 1 )

[ -d $NFSROOT ] && ( echo "The $NFSROOT directory is not empty" ; exit 1 )

mkdir -p $NFSROOT

$DEBOOTSTRAP --include=$DEBOOTSTRAP_INCLUDE_PACKAGES --exclude=$DEBOOTSTRAP_EXCLUDE_PACKAGE lenny $NFSROOT

mkdir $NFSROOT/.nfsroot

echo "127.0.0.1       localhost" > $NFSROOT/etc/hosts

echo "diskless" > $NFSROOT/etc/hostname

echo "nameserver $NAMESERVER" > $NFSROOT/etc/resolv.conf

cat > $NFSROOT/etc/fstab <<EOF
/               /.nfsroot       none    bind,ro         0 0
proc            /proc           proc    defaults        0 0


# copied from /etc/nfsbooted/fstab
/dev/ram        /tmp            ramfs   defaults,rw,auto,dev            0 0
/dev/ram1       /var/run        ramfs   defaults,rw,auto,dev            0 0
/dev/ram2       /var/state      ramfs   defaults,rw,auto,dev            0 0
/dev/ram3       /var/lock       ramfs   defaults,rw,auto,dev            0 0
/dev/ram4       /var/account    ramfs   defaults,rw,auto,dev            0 0
/dev/ram5       /var/log        ramfs   defaults,rw,auto,dev            0 0
/dev/ram6       /var/lib/gdm    ramfs   defaults,rw,auto,dev            0 0
/dev/ram7       /var/tmp        ramfs   defaults,rw,auto,dev            0 0

EOF

cat > $NFSROOT/usr/local/bin/kexec_detach <<EOF
#!/bin/sh

kernelPath=$1
initrdPath=$2
shift
shift
destPartAndParams=$@

echo "KEXEC: Make sure no other kernel has been loaded"
kexec -u

echo "KEXEC: Setting Kexec parameters"
kexec -l $kernelPath --initrd=$initrdPath --append="root=$destPartAndParams"

echo "KEXEC: Running Kexec"
/sbin/start-stop-daemon -S -x /usr/local/bin/run_kexec -b
EOF

cat > $NFSROOT/usr/local/bin/run_kexec <<EOF
#!/bin/sh

sleep 1

echo "KEXEC: Sync"
sync

echo "KEXEC: Killing all running processes ..."
[ -x /etc/init.d/sendsigs ] && /etc/init.d/sendsigs stop

echo "KEXEC: Umount all ..."
[ -x /etc/init.d/umountfs ] && /etc/init.d/umountfs stop

echo "KEXEC: Mounting root read-only ..."
[ -x /etc/init.d/umountroot ] && /etc/init.d/umountroot stop

echo "KEXEC: fire !!!!!!!!!!!" kexec -e
EOF
chmod +x $NFSROOT/usr/local/bin/kexec_detach $NFSROOT/usr/local/bin/run_kexec

#chroot $NFSROOT /usr/sbin/configure-debian --all

cd $TMP

[ -f linux-$KERNEL_VERSION.tar.bz2 ] || ( wget $KERNEL_HTTP_ROOT/linux-$KERNEL_VERSION.tar.bz2 || ( echo "The kernel archive cannot be reached" ; exit 1 ) )

tar -xjf linux-$KERNEL_VERSION.tar.bz2

cd linux-$KERNEL_VERSION

KCONFIG_ALLCONFIG=$CONFIG_FILE make allmodconfig

make -j8 bzImage modules

make INSTALL_MOD_PATH=$NFSROOT modules_install
make INSTALL_PATH=$NFSROOT/boot install

cp $NFSROOT/boot/vmlinuz-$KERNEL_VERSION $TFTPROOT

cd ..

rm -rf linux-$KERNEL_VERSION
