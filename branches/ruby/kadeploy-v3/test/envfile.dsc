name : etch test
version : 1
description : Testing environment based on Etch
author : Emmanuel Jeanvoine
tarball : /home/ejeanvoi/etch.tgz|tgz|165a7d8f0f3011f4d576bcb72c9f6286
postinstall : /home/ejeanvoi/work/kadeploy-svn/branches/ruby/kadeploy-v3/test/user_post_install.tgz|tgz|b87aaa88c105248d731ea5fc87fa8dcc|traitement.sh
kernel : vmlinuz-2.6.18-5-686
kernel_params : ETH_DRV=pcnet32 ETH_DEV=eth0 DISK_DRV=ide_disk console=tty0 console=ttyS1,38400n8 ramdisk_size=400000
initrd : initrd.img-2.6.18-5-686
part : /dev/hda3
fdisk_type : 113
filesystem : ext3
user : ejeanvoi
environment_kind : linux
