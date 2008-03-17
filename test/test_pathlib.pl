#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
    }

use lib qw (/home/nancy/bdexheimer/svn/kadeploy/trunk/);
use strict;
use libkadeploy2::pathlib;

my $path="/var/lib/tftpboot/boot/AABBCCDD";

print "original path string = ".$path."\n";

print "strip_leading_slash --> ".libkadeploy2::pathlib::strip_leading_slash($path)."\n";

print "strip_leading_dirs  --> ".libkadeploy2::pathlib::strip_leading_dirs($path)."\n";

my @t = qw(xen kernel initrd);

print "t = ".@t." | @t\n";

