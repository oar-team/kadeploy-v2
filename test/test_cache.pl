#!/usr/bin/perl


BEGIN{
        unshift(@INC, ".");
}

use lib qw (/home/bdexheimer/svn/kadeploy/trunk/cmd/);
use strict;
use libkadeploy2::cache;


if ( libkadeploy2::cache::init_cache("/tmp/") ) { print "cache cree.\n"; }
else { print "cache non cree.\n"; }

my $moncache=libkadeploy2::cache::get_cache_directory();
print "le cache est " . $moncache . "\n";


my @files = ("kernel-2.6.22", "initrd-2.6.22");
my $f1 = @files[0];
my $f2 = @files[1];

# print "### recherche directe ###\n";
# if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
# else { print $f1 . " n'est pas present dans le cache.\n"; }

my $arc = "env.tgz"; 
my $strip = 0;

print "### insertion cache ###\n";
libkadeploy2::cache::put_in_cache_from_archive(\@files, $arc, $strip);

if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
else { print $f1 . " n'est pas present dans le cache.\n"; }

print "### nettoyage cache ###\n";
libkadeploy2::cache::clean_cache();

if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
else { print $f1 . " n'est pas present dans le cache.\n"; }

# print "### purge cache ###\n";
# libkadeploy2::cache::purge_cache();

if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
else { print $f1 . " n'est pas present dans le cache.\n"; }

print "### get_cache_directory_tftprelative ###\n";

my $tftprelative = libkadeploy2::cache::get_cache_directory_tftprelative(1);
print "tftprelative = " . $tftprelative . "\n";

my $dir = libkadeploy2::cache::get_cache_directory();
$dir = $dir .  "/" . $f1;
print "### Age du fichier : " . $f1 . "###\n";
print "-M = " . ( -M $dir ) . "\n";
print "-A = " . ( -A $dir ) . "\n";
print "-C = " . ( -C $dir ) . "\n";
my $dir = libkadeploy2::cache::get_cache_directory();
$dir = $dir .  "/" . $f2;
print "### Age du fichier : " . $f2 . "###\n";
print "-M = " . ( -M $dir ) . "\n";
print "-A = " . ( -A $dir ) . "\n";
print "-C = " . ( -C $dir ) . "\n";


