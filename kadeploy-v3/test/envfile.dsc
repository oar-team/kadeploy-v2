name : etch test
version : 1
description : Testing environment based on Etch
author : Emmanuel Jeanvoine
tarball_file : /home/ejeanvoi/etch.tgz
tarball_kind : tgz
tarball_md5 : 165a7d8f0f3011f4d576bcb72c9f6286
postinstall_file : /home/ejeanvoi/post.tgz
postinstall_kind : tgz
postinstall_md5 : 2b3c9e21e9a1ed70c4d7337617a0c354
kernel : vmlinuz-2.6.18-5-686
kernel_params : ETH_DRV=pcnet32 ETH_DEV=eth0 DISK_DRV=ide_disk console=tty0 console=ttyS1,38400n8 ramdisk_size=400000
initrd : initrd.img-2.6.18-5-686
part : /dev/hda3
fdisk_type : 113
filesystem : ext3
user : ejeanvoi