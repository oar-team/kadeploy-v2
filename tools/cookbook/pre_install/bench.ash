#!/bin/ash

echo "Perform Bonnie hard-disk test..."
/rambin/bin/bonnie++ -d /tmp/ -s 512 -b -r 0 -n 0 -f -q -x 1
echo "Done Bonnie"
