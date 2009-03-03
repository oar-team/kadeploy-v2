#!/usr/bin/perl


# BEGIN { unshift(@INC, "."); }

no lib qw(:ALL .);
no lib qw(/usr/share/perl/5.8/libkadeploy2);
use lib qw(/home/nancy/bdexheimer/svn/kadeploy/trunk);
use strict;
use libkadeploy2::cache;

print "@INC\n";

my @tex = ("xen", "kernel", "initrd");
print "taille = ".scalar(@tex)." \n";
print "@tex \n";

my $path = "xen --- kernel --- initrd kparam1 kparam2=v1 kparam3=v2";
my @mbfiles = split /-{3}/, $path;
my $f;
foreach $f(@mbfiles) {
    print "f = $f\n";
}
my $initrd=@mbfiles[2];
my @mbfiles2 = split /\s+/, $initrd;
foreach $f(@mbfiles2) {
    print "f = $f\n";
}


my $d = "boot/boot2/boot3/kernel";
my $ld = libkadeploy2::pathlib::get_leading_dirs($d);
print "d = ".$d." ld = ".$ld."\n";

my $d = "kernel";
my $ld = libkadeploy2::pathlib::get_leading_dirs($d);
print "d = ".$d." ld = ".$ld."\n";

my $k = "mboot.c32";
if (libkadeploy2::pathlib::check_multiboot("mboot.c32")) {
    print "MULTIBOOT \n";
} else {
	print "NO MULTI\n";
}
if (libkadeploy2::pathlib::check_multiboot("   mboot.c32   ")) {
    print "MULTIBOOT \n";
} else {
	print "NO MULTI\n";
}
if (libkadeploy2::pathlib::check_multiboot("mbo0t;c32")) {
    print "MULTIBOOT \n";
} else {
	print "NO MULTI\n";
}

my @test_valid = ("", "   ", "kernel", "   xen   ");
foreach my $f (@test_valid) {
  if (libkadeploy2::pathlib::is_valid($f)) {
    print "for test = [ $f ] : VALID\n"; 
  } else {
    print "for test = [ $f ] : NOT VALID\n";
  }
}


if ( libkadeploy2::cache::init_cache("/tmp/") ) { print "cache cree.\n"; }
else { print "cache non cree.\n"; }

my $moncache=libkadeploy2::cache::get_cache_directory();
print "le cache est " . $moncache . "\n";

# my @files = ("vmlinuz", "initrd.img");
# my @files = ("boot/vmlinuz", "boot/initrd");
 my @files = ("boot/kernel", "", "boot/initrd", "     ", "boot/xen", "boot/noyau");
# my $arc = "image.tgz"; 
# my $arc = "bug1850.tgz"; 
 my $arc = "env.tgz"; 
my $strip = 1;

my $f1 = @files[0];
my $f2 = @files[1];
my $env_id=908;
my $f1id = $f1.".".$env_id;
my $f2id = $f2.".".$env_id;

# print "### recherche directe ###\n";
# if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
# else { print $f1 . " n'est pas present dans le cache.\n"; }

libkadeploy2::cache::put_in_cache_from_archive(\@files, $arc, $strip, $env_id);

# if ( libkadeploy2::cache::already_in_cache($f1id) ) { print $f1id . " est present dans le cache.\n"; }
# else { print $f1id . " n'est pas present dans le cache.\n"; }

# print "### nettoyage cache ###\n";
# libkadeploy2::cache::clean_cache();

# if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
# else { print $f1 . " n'est pas present dans le cache.\n"; }

# print "### purge cache ###\n";
# libkadeploy2::cache::purge_cache();

# if ( libkadeploy2::cache::already_in_cache($f1) ) { print $f1 . " est present dans le cache.\n"; }
# else { print $f1 . " n'est pas present dans le cache.\n"; }

# print "### get_cache_directory_tftprelative ###\n";

# my $tftprelative = libkadeploy2::cache::get_cache_directory_tftprelative(1);
# print "tftprelative = " . $tftprelative . "\n";

my $dir = libkadeploy2::cache::get_cache_directory();
$f1id = libkadeploy2::pathlib::strip_leading_dirs($f1id);
$dir = $dir .  "/" . $f1id;
print "### Age du fichier : " . $dir . "###\n";
print "-M = " . ( -M $dir ) . "\n";
print "-A = " . ( -A $dir ) . "\n";
print "-C = " . ( -C $dir ) . "\n";
my $dir = libkadeploy2::cache::get_cache_directory();
$f2id = libkadeploy2::pathlib::strip_leading_dirs($f2id);
$dir = $dir .  "/" . $f2id;
print "### Age du fichier : " . $dir . "###\n";
print "-M = " . ( -M $dir ) . "\n";
print "-A = " . ( -A $dir ) . "\n";
print "-C = " . ( -C $dir ) . "\n";


