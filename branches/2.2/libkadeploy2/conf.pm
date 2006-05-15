package libkadeploy2::conf;

use strict;
use warnings;
use libkadeploy2::message;

my @listfiletowrapuser=(
			"kaconsole",
			"kadeploy",
			"kaenvironments",
			"kareboot",
			"karecordenv",
			"migratenv",
			"kastats",
#			"mcat_rsh.pl",
#			"mcat_ssh.pl",
#			"sentinelle.pl",
#			"setup_pxe.pl",
			);			

my @listfiletowraproot=(
			"kaadduser",
			"kadeluser",
			"kanodes",
			"kadatabase",
			"kasetup",
			);




sub chmodconf()
{
    my $self=shift;

    my $kadeployuser=$self->get("deploy_user");
    my $kadeploydir=$self->get("kadeploy2_directory");

    my $kasudowrapperfile="$kadeploydir/bin/kasudowrapper.sh";
    if ($ENV{USER} eq "root")
    {
	system("chmod 400 $deployconf");
	system("chmod 400 $partitionfile");
	system("chmod 400 $nodesfile");
	
	system("chmod 700 $kadeployconfdir");
	system("chown $kadeployuser $kadeployconfdir");   # la conf appartient a deploy
	
	system("chmod 755 $kasudowrapperfile");           # le wrapper doit pouvoir etre ecris par deploy
	system("chown $kadeployuser $kasudowrapperfile"); 
	
	print STDERR "chmod configuration file done\n";
    }
    else
    {
	print STDERR "You must be root ( user : $ENV{USER} )\n";
	exit 1;
    }
}
