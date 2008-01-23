#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
    }

use lib qw (/home/bdexheimer/svn/kadeploy/trunk/cmd/);
use strict;
use libkadeploy2::hexlib;

my $ip="172.16.181.11";
my $ipa;
my $ipb;
my $ipc;
my $ipd;

print "ip = " . $ip . "\n";
($ipa, $ipb, $ipc, $ipd) = split /\./, $ip;
print "ipa = ". $ipa . " ipb = " . $ipb . " ipc = " . $ipc . " ipd = " . $ipd . "\n";

my $iphexalized=libkadeploy2::hexlib::hexalize($ipa) . 
  libkadeploy2::hexlib::hexalize($ipb) . 
  libkadeploy2::hexlib::hexalize($ipc) . 
  libkadeploy2::hexlib::hexalize($ipd);
 
my $tftp_destination_folder = "/var/lib/tftpboot";
my $pxe_tftp_relative_folder = $iphexalized; # for PXE generated files
my $pxe_dest_folder = $tftp_destination_folder . "/" . $pxe_tftp_relative_folder;

print "pxe_dest_folder =" . $pxe_dest_folder . "\n";

