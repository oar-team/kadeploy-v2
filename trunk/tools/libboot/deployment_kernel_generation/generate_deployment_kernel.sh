#!/bin/bash

####################################################################### 
#
# Generation of a deployment kernel/ramdisk :
# 1. Downloads a fresh kernel from kernel.org
# 2. Let the user configure in the kernel
# 3. Builds kernel
# 4. Builds ramdisk and includes the kadeploy ramdisk base
#
# !! Must be run with root rights !!
#
#######################################################################

PATH=/bin:/sbin:/usr/bin:/usr/sbin

# Fill-in these values (absolute path is needed)
KERNEL_CONFIG=""
KERNEL_VERSION=""
OUTPUT_KERNEL="deploy-vmlinuz"
OUTPUT_INITRD="deploy-initrd"
KERNEL_2_6_ARCHIVE_URL="http://www.eu.kernel.org/pub/linux/kernel/v2.6/"

# Ramdisk size in Ko
INITRD_SIZE="40000"
INITRD_ROOTDEVICE="/dev/ram0"

# For // kernel compilation
NUMBER_OF_CPU=1
NUMBER_OF_CORE=1
COMPILATION_PARALLELISM=$(( ${NUMBER_OF_CPU} * ${NUMBER_OF_CORE} ))

# TMP_INITRD=$OUTPUT_INITRD.uncompressed
ROOT_FS="deploy-initrd-base.tar.bz2"
CURRENT_DIR=$( pwd )
TODAY=$( date +%Y%m%d-%H%M-%N )
CURRENT_BUILTDIR="$CURRENT_DIR/built-$TODAY"
TMP_ROOTDIR="/tmp"
TMP_RDDIR="$TMP_ROOTDIR/__buildrd"
TMP_INITRD="$TMP_ROOTDIR/initrd.build"
TMP_KERNELDIR="$TMP_ROOTDIR/__buildkernel"

NCURSES_PKG='libncurses5-dev'
CMD_SEARCH_PKG='apt-cache search'
CMD_INSTALL_PKG='apt-get install'

Die()
{
  echo "(!!) Error : $1" && exit 1
}

Info()
{
  echo "(II) $1 ..."
}

Warn()
{
  echo "(WW) $1 !"
}

CreateInitrd()
{
  local initrd_size=""

  echo -e "\t--- Deployment ramdisk creation ---"
  echo -en "- Enter INITRD size (in Ko / default = 40000 Ko) : "
  read initrdsize
  if [ -z "$initrdsize" ]; then
    initrdsize=${INITRD_SIZE}
  fi
  
  if [ ! -d "$TMP_RDDIR" ]; then
    mkdir "$TMP_RDDIR" || Die "Failed to create $TMP_RDDIR"
  fi

  Info "Creating empty ramdisk file"
  dd if=/dev/zero of=${TMP_INITRD} bs=1024 count=${initrdsize} 2>&1 >/dev/null || Die "Failed to create empty ramdisk FS"
  mkfs.ext2 -F -q ${TMP_INITRD} 2>&1 >/dev/null || Die "Failed to format $TMP_INITRD"
  tune2fs -c 0 ${TMP_INITRD} 2>&1 >/dev/null
  mount -o loop -t ext2 ${TMP_INITRD} ${TMP_RDDIR} || Die "Unable to mount empty ramdisk loopback file"

  Info "Decompressing ramdisk base filesystem"
  tar -xvjf ${CURRENT_DIR}/${ROOT_FS} -C "$TMP_RDDIR/" 2>&1 >/dev/null || Die "Unable to untar ramdisk base FS"
}

GrabKernelArchive()
{
  if [ ! -d "$TMP_KERNELDIR" ]; then 
    mkdir "$TMP_KERNELDIR" || Die "Failed to create $TMP_KERNELDIR"
  fi
  
  echo -e "\t--- Retrieving linux kernel ---"
  echo -en "- Enter wanted version of kernel : "

  while true; do
    read KERNEL_VERSION
    if [ -f "$CURRENT_DIR/linux-$KERNEL_VERSION.tar.bz2" ]; then
      echo "(II) linux-$KERNEL_VERSION already grabbed"
      break
    else
      wget --verbose --progress=bar "${KERNEL_2_6_ARCHIVE_URL}/linux-${KERNEL_VERSION}.tar.bz2"
      if [ "$?" -eq 0 ]; then
        break
      else
        echo 
        echo "(WW) Previous entry is not a valid version !"
        echo -en "- Please enter a valid kernel version number : "
      fi
    fi
  done
  
  Info "Decompressing kernel archive"
  ( bzip2 -cd $CURRENT_DIR/linux-${KERNEL_VERSION}.tar.bz2|tar -C $TMP_KERNELDIR -xvf - 2>&1 >/dev/null ) || Die "Failed to decompress kernel archive"

}

CheckNcurses()
{
  local installed = $( "${CMD_SEARCH_PKG}" "${NCURSES_PKG}" )
  ( [ -z "$installed" ] && "${CMD_INSTALL_PKG}" "${NCURSES_PKG}" ) || Die "Failed to install ${NCURSES_PKG}"
}

BuildKernel()
{  
  echo -e "\t--- Kernel build ---"

  echo -en "- Enter pathname to a kernel config file : "
  read configfile
  if [ -z "$configfile" ]; then
    CheckNcurses
    echo "(II) Trying to launch kernel configuration utility..."
    ( cd $TMP_KERNELDIR/linux-${KERNEL_VERSION} && make menuconfig ) || Die "Failed to run kernel menuconfig"
    cp $TMP_KERNELDIR/linux-${KERNEL_VERSION}/.config ${CURRENT_DIR}/config-${KERNEL_VERSION}
  else
    while true; do
      if [ ! -f "$configfile" ]; then
        echo
        echo "(WW) $configfile is not a valid config file."
        echo -en "- Please enter a valid kernel config file : "
	read $configfile
      else
        cp ${configfile} ${TMP_KERNELDIR}/linux-${KERNEL_VERSION}/.config
        break
      fi
    done
 fi
  
  cd ${TMP_KERNELDIR}/linux-${KERNEL_VERSION}/
  Info "Making bzImage"
  make -j$COMPILATION_PARALLELISM bzImage 2>&1 >/dev/null || Die "Failed to make bzImage"
  
  Info "Making modules"
  make -j$COMPILATION_PARALLELISM modules 2>&1 >/dev/null || Die "Failed to make modules"
  
  Info "Making kernel install to ${CURRENT_BUILTDIR}"
  INSTALL_PATH=$CURRENT_BUILTDIR make install  || Die "Failed to make kernel installation in $CURRENT_BUILTDIR"

  Info "Making modules install to ${TMP_RDDIR}"
  INSTALL_MOD_PATH=$TMP_RDDIR make modules_install || Die "Failed to make modules installation in $TMP_RDDIR"

  cp ${CURRENT_BUILTDIR}/vmlinuz-${KERNEL_VERSION} ${CURRENT_BUILTDIR}/${OUTPUT_KERNEL}-${KERNEL_VERSION}
  rdev ${CURRENT_BUILTDIR}/${OUTPUT_KERNEL}-${KERNEL_VERSION} ${INITRD_ROOTDEVICE}
  cd "${TMP_RDDIR}/lib/modules" && depmod -b ${TMP_RDDIR} ${KERNEL_VERSION}
  
}

BuildInitrd()
{
  cd ${CURRENT_DIR} && umount ${TMP_RDDIR}
  cat ${TMP_INITRD} | gzip -9 -c > "${CURRENT_BUILTDIR}/${OUTPUT_INITRD}-${KERNEL_VERSION}"
}


CleanOut()
{
  echo -e "\t--- Cleaning out temporary files ---"

  local mounted=$( mount|grep $TMP_RDDIR )
  if [ -n "$mounted" ]; then
    ( cd ${CURRENT_DIR} && umount ${TMP_RDDIR} ) || Warn "Failed to unmount ${TMP_RDDIR}"
  fi
  rm -rf $TMP_KERNELDIR || Warn "Failed to remove kernel build directory $TMP_KERNELDIR"
  rm -rf $TMP_RDDIR || Warn "Failed to remove initrd build directory $TMP_RDDIR"
  # rm -f "${CURRENT_BUILTDIR}/vmlinuz*" 2>&1 >/dev/null || Warn "Failed to rm ${CURRENT_BUILTDIR}/vmlinuz* files" 
  # rm -f "${CURRENT_BUILTDIR}/System.map*" 2>&1 >/dev/null || Warn "Failed to rm ${CURRENT_BUILTDIR}/System.map* files"
  
}

Exit_handler()
{
  # Disables signals trap
  # Ctrl-C / INTerrupt
  trap 2 
  # TERMinate
  trap 15
  
  echo
  CleanOut && Die "Script interrupted ; exiting ..."
}

Main()
{
  # For INT/TERM signals handling
  trap Exit_handler 2 15
  
  if [ ! -d "$CURRENT_BUILTDIR" ]; then
    mkdir ${CURRENT_BUILTDIR}
  fi

  # Builds deployment environment
  CreateInitrd  
  GrabKernelArchive 
  BuildKernel
  BuildInitrd
  
  # Cleaning temporary files and mount
  CleanOut
  
  exit 0
  
}

Main $*

