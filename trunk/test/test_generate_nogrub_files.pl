#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
    }

use lib qw (/home/bdexheimer/svn/kadeploy/trunk/cmd/);
use strict;
use libkadeploy2::bootlib;

my $tftpdir = "/var/lib/tftpboot/boot/AC10B50B";
print "tftpdir = " . $tftpdir . "\n";
$tftpdir =~ s/\/[^\/]*$//;
print "tftpdir = " . $tftpdir . "\n";


my $kernel = "/boot/vmlinuz-2.8.34";
print "kernel = " . $kernel . "\n";
$kernel =~ s/.*\/([^\/]*)$/$1/;
print "kernel = " . $kernel . "\n";

my $img_archive = "/home/nancy/bdexheimer/images/etch-x64-base-xen-1.0.tgz";
my $img_kernel = "mboot.c32";
my $img_initrd = "/boot/xen-3.0.3-1-amd64.gz dom0_mem=262144 --- /boot/vmlinuz-2.6.18-6-xen-amd64 --- /boot/initrd.img-2.6.18-6-xen-amd64";
my $dest_dir = "/var/lib/tftpboot/boot/AABBCC01";
my $firstipx = "AABBCC01";
my $firstnode = 1;
my $envid = 123;

libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode, $envid);

my $f = "xen kernel initrd";
my @tf = qw/ xen kernel initrd /;
my $f2 = shift(@tf);
print "f = ".$f." tf = ".@tf." f2 = ".$f2."\n";

# my $dest_dir = "/var/lib/tftpboot/AABBCC02";
# my $firstnode = 0;
# libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode);

# my $dest_dir = "/var/lib/tftpboot/AABBCC03";
# my $firstnode = 0;
# libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode);
