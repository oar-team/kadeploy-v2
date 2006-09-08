package libkadeploy2::kaconsole;

use File::Copy;
use Getopt::Long;
use libkadeploy2::deployconf;
use libkadeploy2::deploy_iolib;
use libkadeploy2::rights_iolib;
use libkadeploy2::command;
use libkadeploy2::message;
use libkadeploy2::karights;
use libkadeploy2::sudo;
use libkadeploy2::nodelist;
use strict;
use warnings;

sub check_options();
sub check_rights();

my $exitcode;
my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }

my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();

my $sudo_user=libkadeploy2::sudo::get_sudo_user();
if (! $sudo_user) { $sudo_user=libkadeploy2::sudo::get_user(); }

my $kadeploydir=$conf->get("kadeploy2_directory");
my $righttocheck="CONSOLE";

sub new()
{
    my $self;
    $self=
    {
	nodelist => 0,
    };
    bless $self;
    return $self;
}


sub run()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $node;
    my $cmd;

    if (! $self->check_options()) { return 1; }
    if (! $self->check_rights()) { $message->message(2,"$sudo_user not allowed to kaconsole  ".$nodelist->get_str()); exit 1; }
    
    $node=$nodelist->get_node(0);
    $cmd="/etc/kadeploy/nodes/".$node->get_name()."/command/console";
    
    $exitcode=system($cmd);
    $exitcode=$exitcode/256;
    return $exitcode;
}

################################################################################

sub get_options_cmdline()
{
    my $self=shift;
    my $getopt=0;
    my $ref_node_list;
    my @node_list;
    my $nodelist=0;
    my $help=0;

    $getopt=GetOptions(
		       'help!'         => \$help,
		       'h!'            => \$help,
		       
		       'm=s'           => \@node_list,
		       'machine=s'     => \@node_list,
		       );	
    if (! $getopt) { $errorhandler->wrong_parameters_commandline(); }
        if (! $help) 
    {	
	if (@node_list)     
	{ 
	    $nodelist=libkadeploy2::nodelist::new(); 
	    $nodelist->loadlist(\@node_list);  
	}	
	$self->{nodelist}=$nodelist;	
    }    
}


sub check_rights()
{
    my $self=shift;
    my $sudo_user=libkadeploy2::sudo::get_sudo_user();
    my $karights=libkadeploy2::karights::new();
    my $nodelist=$self->{nodelist};
    my $ok=0;

    $karights->set_nodelist($nodelist);

    if (! $sudo_user) { $sudo_user=libkadeploy2::sudo::get_user(); }

    if ( $sudo_user eq "root" ||
	 $sudo_user eq $conf->get("deploy_user")
	 )
    {
	$ok=1;
    }
    if ($karights->check_rights($righttocheck))
    { 	$ok=1;     }	
    return $ok;
}

sub check_options()
{
    my $self=shift;
    my $ok=1;
    my $nodelist=$self->{nodelist};

    if (!$nodelist)
    {
	$message->kaconsole_help();
	exit 0;
    }
    return $ok;
}

