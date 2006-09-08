package libkadeploy2::kareboot;

use File::Copy;
use Getopt::Long;
use libkadeploy2::deployconf;
use libkadeploy2::deploy_iolib;
use libkadeploy2::command;
use libkadeploy2::message;
use libkadeploy2::nodelist;
use libkadeploy2::cmdline;
use libkadeploy2::karights;
use libkadeploy2::sudo;
use libkadeploy2::nodelist;
use strict;
use warnings;

sub execcmd($$$);
sub check_options;
sub check_right($);


my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }

my $sudo_user=libkadeploy2::sudo::get_sudo_user();
if (! $sudo_user) { $sudo_user=libkadeploy2::sudo::get_user(); }

my $ok=1;
my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();
my $kadeploydir=$conf->get("kadeploy2_directory");
my $righttocheck="REBOOT";

sub set_nodelist($)   { my $self=shift; $self->{nodelist}=shift; }

sub new()
{
    my $self;
    $self=
    {
	soft => 0,
	hard => 0,
	nodelist => 0,
	noreboot => 0,
	deploy   => 0,	
	nodelist => 0,
    };
    bless $self;
    return $self;
}

sub run()
{
    my $self=shift;
    my $ref_node_list;
    my @node_list;
    my $nodelist=$self->{nodelist};
    my $node;
    my $verbose=$self->{verbose};
    if (! $self->check_options()) { return 1; }
    

    if (!$self->check_right($righttocheck))
    { 
	$message->message(2,"$sudo_user not allowed to $righttocheck ".$nodelist->get_str()); 
	exit 1; 
    }
    
    
    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    
    foreach $node (@node_list)
    {
	if ($self->{soft})
	{
	    if (! $self->execcmd($node,"softboot",$verbose))
	    { $ok=0; }
	}    
	if ($self->{hard})
	{
	    if (! $self->execcmd($node,"hardboot",$verbose))
	    { $ok=0; }
	}
	if ((! $self->{soft} ) &&
	    (! $self->{hard}))
	{
	    if (! $self->execcmd($node,"softboot",$verbose))
	    {
		if (! $self->execcmd($node,"hardboot",$verbose))
		{		
		    $ok=0;
		}
	    }
	}
    }
    if (! $ok) { return 1; }
    else       { return 0; }
}

################################################################################

sub get_options_cmdline()
{
    my $self=shift;
    my $getopt=0;
    my $ref_node_list;
    my @node_list;
    my $node_file;
    my $node;
    my $env;
    my $soft;
    my $hard;
    my $deploy;
    my $noreboot;
    my $device;
    my $verbose=0;
    my $refnodelist;
    my $command;
    my $cmd;
    my $nodeshell;
    my $nodelist=0;
    my $help=0;

    $getopt=GetOptions(
		       'm=s'           => \@node_list,
		       'machine=s'     => \@node_list,
		       'f=s'           => \$node_file,
		       
		       'e=s'           => \$env,
		       'environment=s' => \$env,
		       
		       's'             => \$soft,
		       'soft'          => \$soft,
		       
		       'h'             => \$hard,
		       'hard'          => \$hard,
		       
		       'd'             => \$deploy,
		       'deploy'        => \$deploy,
		       
		       'n'             => \$noreboot,
		       'noreboot'      => \$noreboot,
		       
		       'p=s'           => \$device,
		       'partition=s'   => \$device,
		       
		       'verbose'       => \$verbose,
		       'v'             => \$verbose,
		       
		       'help!'         => \$help,
		       'h!'            => \$help,
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

sub check_options()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $ok=1;
    if ($self->{help}) { $message->kareboot_help(); exit 0; }
  
    if (!$nodelist)
    {
	$message->kareboot_help();
	$ok=0;
    }
    
    if ($self->{soft} && $self->{hard})
    {
	$message->message(2,"soft and hard reboot options are exclusive");
	$message->message(2,"please select only one of them at once");
	$ok=0;
    }
    
    if (($self->{noreboot} && $self->{soft}) || 
	($self->{noreboot} && $self->{hard}))
    {
	$message->message(2,"no reboot and soft or hard reboot should be exclusive");
	$message->message(2,"please select only one of them at once");
	$ok=0;
    }
    
    if (($self->{deploy} && $self->{env}) || 
	($self->{deploy} && $self->{device}) || 
	($self->{env}    && $self->{device}))
    {
	$message->message(2,"please select EITHER a deployment reboot OR a reboot on a partition OR a reboot on an environment");
	$ok=0;
    }

    return $ok;
}



sub check_right($)
{
    my $self=shift;
    my $righttocheck=shift;
    my $karights=libkadeploy2::karights::new();
    $karights->set_nodelist($self->{nodelist});
    return $karights->check_rights($righttocheck);
}



sub execcmd($$$)
{
    my $self=shift;
    my $node=shift;
    my $cmdnode=shift;
    my $verbose=shift;
    my $refnodelist;
    my @node_list;
    my $ok=1;
    my $nodelist=$self->{nodelist};
    my $cmd;
    my $command;

    $refnodelist=$nodelist->get_nodes();
    @node_list=@$refnodelist;


    $cmd=$conf->getpath_cmd("kaexec")." --confcommand -m ".$node->get_name()." -c $cmdnode";
    $command=libkadeploy2::command::new();
    $command->set_command($cmd);
    $command->set_timeout(30);
    if ($verbose) { $command->set_verbose(); }
    $command->exec();
    if (! $command->get_status())
    { 
	$message->message(0,$sudo_user." user ".$cmdnode." ".$node->get_name()." fail");
	$ok=0; 
    }
    else
    {
	$message->message(0,$sudo_user." user ".$cmdnode." ".$node->get_name()." success");
    }    
    return $ok;
}



1;
