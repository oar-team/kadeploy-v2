#!/usr/bin/perl
#This program is derived from oar project's sources (http//:oar.imag.fr)
#

use Fcntl; 

my $sudoers = "/etc/sudoers";
my $sudoerstmp = "/etc/sudoers.tmp"; 
my $kadeploy_tag="# DO NOT REMOVE, needed by Kadeploy packages"; 
my $struct=pack("ssll", F_WRLCK, SEEK_CUR, 0, 0);

my $bindir="@ARGV/bin/";
my $sbindir="@ARGV/sbin/";

sysopen (SUDOERS, $sudoers, O_RDWR|O_CREAT, 0440) or die "sysopen $sudoers: $!"; 
fcntl(SUDOERS, F_SETLK, $struct) or die "fcntl: $!";
sysopen (SUDOERSTMP, "$sudoerstmp", O_RDWR|O_CREAT, 0440) or die "sysopen $sudoerstmp: $!"; 
print SUDOERSTMP grep (!/$kadeploy_tag/, <SUDOERS>); 
print SUDOERSTMP <<TOTO;
##BEGIN $kadeploy_tag 
Defaults>deploy          env_reset,env_keep = "PWD PERL5LIB DISPLAY" $kadeploy_tag 
Cmnd_Alias DEPLOYCMDUSER = $bindir/kaconsole, $bindir/kadeploy, $bindir/kaenvironments, $bindir/kareboot, $bindir/karecordenv, $bindir/migratenv, $bindir/karemote $kadeploy_tag 
ALL ALL=(deploy) NOPASSWD: DEPLOYCMDUSER $kadeploy_tag 
##END$kadeploy_tag 
TOTO
close SUDOERSTMP or die "close $$sudoerstmp: $!";
rename "/etc/sudoers.tmp", "/etc/sudoers" or die "rename: $!"; 
close SUDOERS or die "close $$sudoers: $!";

