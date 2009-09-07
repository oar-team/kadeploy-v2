#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
    }

use lib qw (/home/bdexheimer/svn/kadeploy/trunk/cmd/);
use strict;

my $current_initrd = "AC10B50A/initrd.img-2.6.21.3 root=/dev/hda2 console=tty0";
print "initrd = " . $current_initrd . "\n";
$current_initrd    =~ s/^[a-fA-F0-9]{8}\/(.*)$/$1/;
print "initrd = " . $current_initrd . "\n";


