#
# Harddrive preparation script
#

# Format the harddrive
if [ $DO_FDISK ] ; then
	echo "Partitioning ${HARDDRIVE}" > ${TTYS}
	cat  ${FDISKFILE}  | fdisk ${HARDDRIVE}
fi

# Manage swap partition
if  [ $SWAP_FORMAT ]; then
	echo "Formating swap on partition ${SWAP_PART}" > ${TTYS}
	mkswap ${SWAP_PART}
fi

# Manage /tmp
if [ $TMP_FORMAT ]; then
	echo "Formatting /tmp on device ${TMP_DEV} with fs ${TMP_FSTYPE}" > ${TTYS}
	mkfs -t mkfs -t ${TMP_FSTYPE} ${TMP_FSTYPE_OPTIONS} ${TMP_PART}
	mount ${TMP_PART} /mnt/tmp
	# deactivate mount count at mount
	tune2fs -c 0 ${TMP_PART}
	# add stickybit on tmp
	chmod 1777 /mnt/tmp
	umount /mnt/tmp
fi
