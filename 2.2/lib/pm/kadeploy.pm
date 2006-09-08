package libkadeploy2::kadeploy;
use strict;
use warnings;
use Getopt::Long;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::environment;
use libkadeploy2::deploy_iolib;
use libkadeploy2::deployconf;
use libkadeploy2::nodelist;
use libkadeploy2::command;
use libkadeploy2::sudo;
use libkadeploy2::environment;
use libkadeploy2::kareboot;
use libkadeploy2::kadeployenv;
use libkadeploy2::kachecknodes;

sub execcmd($);

my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();
my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }
my $kadeploydir=$conf->get("kadeploy2_directory");
my $kareboot=$conf->getpath_cmd("kareboot");
my $kachecknodes=$conf->getpath_cmd("kachecknodes");
my $deployenv_name=$conf->get("deployenv_name");
my $deployenv_login=$conf->get("deployenv_login");
my @node_list;
my $default_disknumber=$conf->get("default_disk_number");
my $default_partnumber=$conf->get("default_partition_number");
my $sudo_user=libkadeploy2::sudo::get_sudo_user();
my $timetosleep=60;


sub new()
{
    my $self;
    $self=
    {
	login           => "",
	envname         => "",
	nodelist        => "",
	environment     => "",
	help            => 0,
	bootfromnetwork => 0,
	bootfromdisk    => 0,
	noreboot        => 0,
	connector       => "ssh",

	verbose         => 0,
	debug           => 0,
    };
    bless $self;
    return $self;
}

sub get_options_cmdline()
{
    my $self=shift;
    my $login;
    my $help;
    my $verbose=0;
    my $envname;
    my $environment;
    my $nodelist;
    my $basefile;
    my $partitionfile;
    my $cmd;
    my $ok=1;
    my $noreboot=0;
    my $command;
    my $debug;
    my $envfile;
    my $disknumbercmdline;
    my $partnumbercmdline;
    
    my $getopt=GetOptions(
			  'm=s'                  => \@node_list,
			  'machine=s'            => \@node_list,
			  
			  'disknumber=i'         => \$disknumbercmdline,
			  'd=i'                  => \$disknumbercmdline,
			  'partnumber=i'         => \$partnumbercmdline,
			  'p=i'                  => \$partnumbercmdline,
			  
			  'login=s'              => \$login,
			  'l=s'                  => \$login,
			  
			  'environment=s'        => \$envname,
			  'e=s'                  => \$envname,
			  
			  'h!'                   => \$help,
			  'help!'                => \$help,
			  'v!'                   => \$verbose,
			  'verbose!'             => \$verbose,
			  'debug!'               => \$debug,
			  'noreboot!'            => \$noreboot,
			  );

    if (! $getopt) { $errorhandler->wrong_parameters_commandline(); }
    if ($verbose)  { $errorhandler->set_verbose(); }
    if ($debug)    { $message->set_debug(); }

    $self->{disknumbercmdline}=$disknumbercmdline;
    $self->{partnumbercmdline}=$partnumbercmdline;
    $self->{help}=$help;  

    if ($noreboot) { $self->{noreboot}=1; }
    if ($verbose)  { $self->{verbose}=1;  }

    foreach my $arg (@ARGV) { if ($arg =~ /([a-zA-Z0-9_]+)\@([a-zA-Z0-9\._]+)/) { $login=$1; $envname=$2; } }

    $self->{login}=$login;
    $self->{envname}=$envname;

    if (! $help) 
    {	
	if (@node_list)     
	{ $nodelist=libkadeploy2::nodelist::new(); $nodelist->loadlist(\@node_list);  }	
	$self->{nodelist}=$nodelist;
    }
    return $getopt;
}


sub check_options()
{
    my $self=shift;

    my $help=$self->{help};
    my $disknumber=$self->{disknumbercmdline};
    my $partnumber=$self->{partnumbercmdline};
    my $login=$self->{login};
    my $envname=$self->{envname};
    my $verbose=$self->{verbose};
    my $ok=1;

    if ($help)            { $message->kadeploy_help(); exit 0; }   

    if (! $disknumber)    { $message->missing_cmdline(2,"disknumber needed"); return 0; }
    if (! $partnumber)    { $message->missing_cmdline(2,"partnumber needed"); return 0; }
    if (! ($disknumber =~ /^\d+$/)) { $message->missing_cmdline(2,"partnumber is a number begining at 1"); return 0; }
    if (! ($partnumber =~ /^\d+$/)) { $message->missing_cmdline(2,"partnumber is a number begining at 1"); return 0; }
    if (! $login)         { $message->missing_cmdline(2,"user name needed"); return 0; }
    if (! $envname)       { $message->missing_cmdline(2,"environment name needed"); return 0; }
    return $ok;
}

sub run()
{
    my $self=shift;   
    my $environment=$self->{environment};
    my $envfile=$self->{envfile};
    my $envname=$self->{envname};
    my $login=$self->{login};
    my $nodelist=$self->{nodelist};
    my $verbose=$self->{verbose};		       
    my $kadeployenv;
    my $kachecknodes;
    my $cmd;
    my $ok=1;
    my $kareboot;
    my @type_list;
    my $disknumber=$self->{disknumbercmdline};
    my $partnumber=$self->{partnumbercmdline};

   
    if (! $self->check_options()) { $message->message(2,"Something wrong while checking option"); return 1; }
    $environment=libkadeploy2::environment::new();
    $environment->set_name($envname);
    $environment->set_user($login);    

    
    if (! $environment->get_descriptionfile_fromdb()) { $message->message(2,"Environment not registred in db...");    exit 1; }
    if (! $environment->load())                       { $message->erroropeningfile(2,$environment->get_descriptionfile()); exit 1; 	}
    if (! $environment->get("deploytype"))            { $message->message(2,"Your configuration file doesn't contain deploytype"); exit 1; 	}
    $envfile=$environment->get_descriptionfile();	
    $ok=1;

    $kareboot=libkadeploy2::kareboot::new();
    $kareboot->set_nodelist($nodelist);

    $kadeployenv=libkadeploy2::kadeployenv::new();
    if ($self->{verbose}) { $kadeployenv->set_verbose(); }

    $message->message(-1,"Using $deployenv_login\@$deployenv_name");
    $kadeployenv->set_envname($deployenv_name);
    $kadeployenv->set_login($deployenv_login);
    $kadeployenv->set_nodelist($nodelist);    
    if ($kadeployenv->run()==0) 
    { 
	$errorhandler->exit_failto("$sudo_user Fail with pxe for node ".$nodelist->get_str()); 
    }
    
    #WAITING FOR NODE IN DEPLOYMENT STATE
    $kachecknodes=libkadeploy2::kachecknodes::new();
    @type_list=("MCAT");
    $kachecknodes->set_type_list(\@type_list);
    $kachecknodes->set_nodelist($nodelist);
    $kachecknodes->set_check();
    if ($kachecknodes->run()!=0)
    {
	if ($kareboot->run()!=0) { $errorhandler->exit_failto("Fail with reboot command"); }	
	$message->message(-1,"Sleeping $timetosleep");
	sleep($timetosleep);
    }
    
    $kadeployenv->set_envname($envname);
    $kadeployenv->set_login($login);
    $kadeployenv->set_partnumber($partnumber);
    $kadeployenv->set_disknumber($disknumber);

    if ($kadeployenv->run()!=0) 
    { $errorhandler->exit_failto("Fail to deploy");  }
    else { $ok=0; }
    
    #FINAL REBOOT 
    if ($kareboot->run()!=0)
    { $errorhandler->exit_failto("Fail whith reboot"); }
    
    $message->message(-1,"Sleeping $timetosleep");
    sleep($timetosleep);


    $kachecknodes->set_type("ICMP");
    if (! $kachecknodes->run()!=0) 
    { $errorhandler->exit_failto("Fail waiting for node"); $ok=0; }
    
    if (! $ok) { $message->message(0,"Deployment failure"); return 1; }
    else { $message->message(0,"Deployment success"); return 0; }
}


################################################################################

sub execcmd($)
{
    my $self=shift;
    my $cmd=shift;
    my $command=libkadeploy2::command::new();

    $command->set_command($cmd);
    $command->set_timeout(1000);
    #if ($self->{verbose}) { $command->set_verbose(); }

    $command->exec();
    return $command->get_status();
}
