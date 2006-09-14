package libkadeploy2::kaexec;

use Getopt::Long;
use libkadeploy2::deployconf;
use libkadeploy2::parallelcommand;
use libkadeploy2::message;
use libkadeploy2::cmdline;
use libkadeploy2::remoteparallelcommand;
use libkadeploy2::errorhandler;
use strict;
use warnings;

sub kaexecconf();
sub kaexec();
sub kaexeccommand();

my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();
my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }


my $prefixpath="/etc/kadeploy/nodes";

sub new()
{
    my $self=
    {
	nodelist     => 0,
	confcommand  => 0,
	nodecommand  => 0,
	cmd          => "",
	login        => "",
	timeout      => 0,
    };
    bless $self;
    return $self;
}

sub run()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $errcode=0;
    if ($nodelist)
    {

        #CONFCOMMAND
	if ($self->{confcommand} &&          
	    $self->{cmd} 	
	    )
	{
	    if ($self->kaexecconf())		
	    { $errcode=0; }
	    else 	
	    { $errcode=1; }
	}

        #NODECOMMAND
	elsif ($self->{nodecommand} &&
	       $self->{cmd}         && 
	       $self->{login} 
	       )
	{
	    if ($self->kaexeccommand())
	    { $errcode=0; }
	    else
	    { $errcode=1; }
	}
	else
	{
	    $message->message(2,"Not enough argument");
	    $errorhandler->wrong_parameters_commandline();
	}
    }
    return $errcode;
}



################################################################################

sub get_options_cmdline()
{
    my $self=shift;
    my $verbose=0;
    my $confcommand;
    my $help;
    my $login;
    my $connector;
    my $timeout;
    my @node_list=();   
    my $refnodelist;
    my $nodename;
    my $nodelist;
    my $hostfile;
    my $cmd;
    my $command;
    my $nodecommand;
    my $cmdpath;
    my $errcode;
    my $scriptfile;

    my $debug=0;
    my $getopt=0;

    $getopt=GetOptions('m=s'               => \@node_list,
		       'machine=s'         => \@node_list,

		       'f=s'               => \$hostfile,
		       
		       'c=s'               => \$cmd,
		       'command=s'         => \$cmd,
		       's=s'               => \$scriptfile,
		       'script=s'          => \$scriptfile,
		       
		       'confcommand!'      => \$confcommand,
		       'nodecommand!'      => \$nodecommand,
		       'h!'                => \$help,
		       'help!'             => \$help,
		       'v!'                => \$verbose,
		       'verbose!'          => \$verbose,
		       'login=s'           => \$login,
		       'l=s'               => \$login,
		       'connector=s'       => \$connector,
		       'timeout=s'         => \$timeout,
		       't=s'               => \$timeout,
		       'debug!'            => \$debug,
		       );

    if (! $getopt)    { $errorhandler->wrong_parameters_commandline(); }
    if ($help)        { $message->kaexec_help(); exit 0; }
    if ($debug)       { $message->set_debug(); }
    if ($hostfile)    { $nodelist=libkadeploy2::cmdline::loadhostfileifexist($hostfile);  }

    if (@node_list)     
    { $nodelist=libkadeploy2::nodelist::new(); $nodelist->loadlist(\@node_list);  }	
    $self->{nodelist}=$nodelist;

    $self->{cmd}=$cmd;

    if ($confcommand) { $self->{confcommand}=1;   }
    if ($nodecommand) { $self->{nodecommand}=1;   }
    if (! $nodecommand && ! $confcommand)           { $self->{nodecommand}=1; }
    if (! $login)     { $self->{login}="root";    } else { $self->{login}=$login }
    if (! $connector) { $self->{connector}="ssh"; }
    if (! $timeout)   { $self->{timeout}=10;      }
    if ($verbose)     { $self->{verbose}=1;       }    
}


sub kaexeccommand()
{
    my $self=shift;
    my $connector=$self->{connector};
    my $login=$self->{login};
    my $nodelist=$self->{nodelist};
    my $cmd=$self->{cmd};
    my $timeout=$self->{timeout};
    my $verbose=$self->{verbose}; 
    my $parallellauncher=$conf->get("parallel_launcher");

    my $refnodelist;
    my @node_list;
    my $remoteparallelcommand=libkadeploy2::remoteparallelcommand::new();


    $remoteparallelcommand->set_connector($connector);
    $remoteparallelcommand->set_parallellauncher($parallellauncher);
    $remoteparallelcommand->set_login($login);
    $remoteparallelcommand->set_nodelist($nodelist);
    $remoteparallelcommand->set_command($cmd);
    $remoteparallelcommand->set_timeout($timeout);
    if ($verbose) { $remoteparallelcommand->set_verbose(); }
    $remoteparallelcommand->set_openstdout();
    
    if ($remoteparallelcommand->exec())
    { return 1; }
    else
    { return 0; }   
}



sub kaexecconf()
{
    my $self=shift;
    my $login=$self->{login};
    my $nodelist=$self->{nodelist};
    my $cmd=$self->{cmd};
    my $timeout=$self->{timeout};
    my $verbose=$self->{verbose};
    my @node_list;
    my @cmdlist;
    my $refcmdlist;
    my $ok=1;
    my $pcommand;
    my $refnodelist;
    my $node;
    my $nodename;
    my $cmdpath;
    my $errcode;

    $refnodelist=$nodelist->get_nodes();
    @node_list=@$refnodelist;
    
    foreach $node (@node_list)
    {
	$nodename=$node->get_name();
	$cmdpath=$prefixpath."/".$nodename."/command/".$cmd; 
	if (-e $cmdpath)
	{
	    @cmdlist=(@cmdlist,$cmdpath);
	}
	else
	{
	    $message->commandnodenamefailed(2,$cmd,$nodename);
	    $ok=0;
	}
    }

    $refcmdlist=\@cmdlist;
    $pcommand=libkadeploy2::parallelcommand::new();
    $pcommand->set_timeout(10);
    if ($verbose) { $pcommand->set_verbose(); }
    $errcode=$pcommand->execparallel($refcmdlist);
    if ($ok==0) { $errcode=0; }
    return $errcode;
}

1;
