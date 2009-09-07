#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
    }

use lib qw (/home/bdexheimer/svn/kadeploy/trunk/cmd/);
use strict;
use libkadeploy2::hexlib;

my $ip = "172.16.181.11";
print "ip = " . $ip . "\n";
my $ipx=libkadeploy2::hexlib::gethostipx($ip);
print "ipx = " . $ipx . "\n";

