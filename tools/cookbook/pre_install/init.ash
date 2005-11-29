#!/bin/ash

# accelerons le disque
modprobe serverworks
hdparm -c1 -d1 /dev/hda

# partitionnement et formattage des partitions
#/rambin/format.ash

# perform hard-disk benches
/rambin/bench.ash

# Duke
