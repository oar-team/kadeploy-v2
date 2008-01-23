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

my $img_archive = "/grid5000/images/sid32bits-ejeanvoine8.tgz";
my $img_kernel = "/boot/vmlinuz-2.6.21.3";
my $img_initrd = "/boot/initrd.img-2.6.21.3";
my $dest_dir = "/var/lib/tftpboot/AABBCC01";
my $firstipx = "AABBCC01";
my $firstnode = 1;
libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode);

my $dest_dir = "/var/lib/tftpboot/AABBCC02";
my $firstnode = 0;
libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode);

my $dest_dir = "/var/lib/tftpboot/AABBCC03";
my $firstnode = 0;
libkadeploy2::bootlib::generate_nogrub_files($img_archive, $img_kernel, $img_initrd, $dest_dir, $firstipx, $firstnode);
