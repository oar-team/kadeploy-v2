#!/bin/bash
#Must be run as root

#Fill-in these values (absolute path is needed)
KERNEL_CONFIG=
KERNEL_VERSION=
OUTPUT_KERNEL=
OUTPUT_INITRD=
NUMBER_OF_CPU=2 #used to compile the kernel
INITRD_SIZE=40000 #in Ko

ROOT_FS=root_fs.tar.bz2
CURRENT_DIR=`pwd`
COMPILATION_PARALLELISM=$(( $NUMBER_OF_CPU * 2 ))
TMP_INITRD=$OUTPUT_INITRD.uncompressed

#Create initrd
mkdir image
dd if=/dev/zero of=$TMP_INITRD bs=1024 count=$INITRD_SIZE
mkfs.ext2 -F -q $TMP_INITRD
tune2fs -c 0 $TMP_INITRD
mount -o loop $TMP_INITRD image/
cd image; tar xjf ../$ROOT_FS
cd $CURRENT_DIR

#Grab and compile a kernel
mkdir kernel
cd kernel
if [ -d linux-$KERNEL_VERSION ]
then
    echo "Kernel already grabbed"
else
    wget http://www.eu.kernel.org/pub/linux/kernel/v2.6/linux-$KERNEL_VERSION.tar.bz2
    tar xjf linux-$KERNEL_VERSION.tar.bz2
    cd linux-$KERNEL_VERSION
    cp $KERNEL_CONFIG .config
fi
echo "If you want to stop here in order to configure your kernel, enter 'stop'"
read response
[ "$response" == "stop" ] && exit 0

make -j$COMPILATION_PARALLELISM bzImage
make -j$COMPILATION_PARALLELISM modules
INSTALL_PATH=$CURRENT_DIR make install
INSTALL_MOD_PATH=$CURRENT_DIR/image make modules_install

cd $CURRENT_DIR
cp vmlinuz-$KERNEL_VERSION $OUTPUT_KERNEL
rdev $OUTPUT_KERNEL /dev/ram0
cd image/lib/modules
depmod -b $CURRENT_DIR/image $KERNEL_VERSION

cd $CURRENT_DIR
umount image
cat $TMP_INITRD | gzip -9 -c > $OUTPUT_INITRD

#Clean
rm vmlinuz*
rm $TMP_INITRD
rm System.map-$KERNEL_VERSION
rm System.map
rm config
rm -r image
rm -rf kernel
mv config-$KERNEL_VERSION.old config-$KERNEL_VERSION
