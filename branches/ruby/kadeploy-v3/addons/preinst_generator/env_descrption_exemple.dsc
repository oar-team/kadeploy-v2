###
name : env_aurelien
version : 4
description : https://www.grid5000.fr/index.php/Sid-x64-base-1.0
author : Aurélien Cedeyn
tarball : /home/acedeyn/kadeploy-v3-tests/images/sid_x86_64.tgz|tgz
preinstall : /home/acedeyn/preinstall.tgz|tgz|launch.sh
postinstall : /home/rennes/ejeanvoine/sid-x64-base-1.1-post.tgz|tgz|traitement.sh /rambin
kernel : /vmlinuz
kernel_params : console=tty0 console=ttyS0,38400n8
initrd : /boot/initrd.img-2.6.24.3
fdisktype : 83
filesystem : ext3
environment_kind : linux
demolishing_env : 127
