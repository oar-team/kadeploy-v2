#!/bin/ash

# on copie le fichier binroot.img dans /mnt/rambin (ou le ramdisk $RAMBIN
# est deja monte) puis on monte en loop /mnt/rambin/binroot.img dans
# /sysroot/rambin
# /sysroot/rambin/bin et /sysroot/rambin/lib sont pointees par
# /usr/bin et /usr/lib et sont dans le path de ash
# on peut donc les inclure dans les scripts ash sans prefixe!!

HARDDRIVE="/dev/hda"
SWAP="/dev/hda1"
FORMATSWAP=1
TMP="/dev/hda3"
TMPFS="ext2"
FORMATTMP=1
KERNEL="/dev/hda2"
KERNELFS="ext2"
FORMATKERNEL=1
DEST="/dev/hda5"
DESTFS="ext2"
FORMATDEST=1 # on formatte la partition de destination

# on cree le repertoire pour monter la partition des noyaux
mkdir /mnt/kernel

# on partitionne le disque
cat /rambin/fdisk.txt | fdisk ${HARDDRIVE}

# on formatte ce qu'il faut
if [ $FORMATSWAP ]; then
	mkswap ${SWAP}
fi

if [ $FORMATTMP ]; then
        mkfs -t ${TMPFS} ${TMP}
fi

if [ $FORMATKERNEL ]; then
        mkfs -t ${KERNELFS} ${KERNEL}
fi
	
if [ $FORMATDEST ]; then
        mkfs -t ${DESTFS} ${DEST}
fi

# on monte les partitions
mount ${TMP} /mnt/tmp
mount ${DEST} /mnt/dest
mount ${KERNEL} /mnt/kernel

# on n'a plus qu'a attendre le mput!
# Duke
