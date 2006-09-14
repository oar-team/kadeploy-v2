package libkadeploy2::remoteparallelcommand;

use libkadeploy2::command;
use libkadeploy2::remotecommand;
use libkadeploy2::message;
use strict;
use warnings;

my $message=libkadeploy2::message::new();

sub new()
{
    my $self=
    {
	connector        => "ssh",
	parallellauncher => "internal",
	login            => "root",
	nodelist         => 0,
	cmd              => "",
	timeout          => 0,
	verbose          => 0,
	status           => -1,
	openstdout       => 0,
    };
    bless $self;
    $self->check();
    return $self;
}

sub set_connector($)        { my $self=shift; my $args=shift; $self->{connector}=$args; }
sub set_parallellauncher($) { my $self=shift; my $args=shift; $self->{parallellauncher}=$args; }
sub set_timeout($)          { my $self=shift; my $args=shift; $self->{timeout}=$args; }
sub set_login($)            { my $self=shift; my $args=shift; $self->{login}=$args; }
sub set_nodelist($)         { my $self=shift; my $args=shift; $self->{nodelist}=$args; }
sub set_command($)          { my $self=shift; my $args=shift; $self->{cmd}=$args; }
sub set_verbose()           { my $self=shift; $self->{verbose}=1; }
sub set_unverbose()         { my $self=shift; $self->{verbose}=0; }
sub set_openstdout()        { my $self=shift; $self->{openstdout}=1; }

sub check()
{
    my $self=shift;
    my $ok=1;
    if ( !($self->{parallellauncher} eq "dsh") &&
	 !($self->{parallellauncher} eq "DKsentinelle") &&
	 !($self->{parallellauncher} eq "internal"))
    { $message->message(3,"parallellauncher must be : dsh | DKsentinelle | internal"); $ok=0; }
    if ( !($self->{connector} eq "rsh" ) &&
	 !($self->{connector} eq "ssh" ) )
    { $message->message(3,"connector must be : rsh | ssh (#".$self->{cmd}.")"); $ok=0; }
    if (!($self->{timeout}>=0)) { $message->message(3,"timeout :".$self->{timeout}."(must be >= 0)"); $ok=0; }
    return $ok;
}

sub exec()
{
    my $self=shift;
    my $ok=1;
    if (! $self->check()) { $ok=0; }
    if ($ok)
    {
	if ($self->{parallellauncher} eq "dsh")               
	{ $self->{status}=$self->exec_dsh(); }
	
	if ($self->{parallellauncher} eq "DKsentinelle")      
	{ $self->{status}=$self->exec_DKsentinelle(); }
	
	if ($self->{parallellauncher} eq "internal")  
	{ $self->{status}=$self->exec_internal(); }
	
	$ok=$self->{status};
    }
    return $ok;
}

sub exec_dsh()
{
    my $self=shift;
    my $command;
    my $prefixcmd="dsh -c -r ".$self->{connector};
    my $nodelist=$self->{nodelist};
    my $ref_ip_list=$nodelist->get_ip_list();
    my @ip_list=@$ref_ip_list;
    my $ip;
    foreach $ip (@ip_list)
    {device
	$prefixcmd.=" -m ".$self->{login}."@".$ip;
    }
    $prefixcmd.=" -- ".$self->{cmd};
    $command=libkadeploy2::command::new();
    $command->set_command($prefixcmd);
    $command->set_timeout($self->{timeout});
    if ($self->{verbose}) { $command->set_verbose(); }
    return $command->exec();
}

sub exec_DKsentinelle()
{
    my $self=shift;
    my $command;
    my $prefixcmd="DKsentinelle  -c ".$self->{connector}." -l ".$self->{login};
    my $nodelist=$self->{nodelist};
    my $ref_ip_list=$nodelist->get_ip_list();
    my @ip_list=@$ref_ip_list;
    my $ip;
    foreach $ip (@ip_list)
    {
	$prefixcmd.=" -m ".$ip;
    }
    $prefixcmd.=" -- ".$self->{cmd};
    $command=libkadeploy2::command::new();
    $command->set_command($prefixcmd);
    $command->set_timeout($self->{timeout});
    if ($self->{verbose}) { $self->set_verbose(); }
    return $command->exec();
}

sub exec_internal()
{
    my $self=shift;
    my $remotecommand;
    my $ok=0;
    my $nodelist=$self->{nodelist};
    my $node;
    my $i;
    my %pid2cmd;
    my %pid2node;
    my @pid_list;

    #create childs
    for ($i=0;$i<$nodelist->get_numberofnode()+1;$i++)
    {
	my $pid;
	my $exitcode;


	$pid=fork();
	$node=$nodelist->get_node($i);

	#son
	if ($pid==0)
	{
	    $remotecommand=libkadeploy2::remotecommand::new();
	    $remotecommand->set_connector($self->{connector});
	    $remotecommand->set_login($self->{login});
	    $remotecommand->set_node($node);
	    $remotecommand->set_command($self->{cmd});
	    $remotecommand->set_timeout($self->{timeout});
	    if ($self->{openstdout}) { $remotecommand->set_openstdout(); }
	    if ($self->{verbose})    { $remotecommand->set_verbose(); }
	    
	    if ($remotecommand->exec())
	    { $exitcode=0;  }
	    else
	    { $exitcode=1;  }
	    exit $exitcode; #DIIEEEE!!!
	}	
	else
	#dady
	{
	    $pid2cmd{$pid}=$self->{cmd};
	    $pid2node{$pid}=$node->get_name();
	    @pid_list=(@pid_list,$pid);
	}
    }
    $ok=1;
    #remove zombie
    for ($i=0;$i<$nodelist->get_numberofnode()+1;$i++)
    {
	my $pid;
	my $exit_value;
	my $signal_num;
	my $dumped_core;
	
	$pid=$pid_list[$i];
	$pid=waitpid($pid,0);
	if (! $pid) { $message->message(2,"wait syscall fail");  }
	else
	{
	    $exit_value  = $? >> 8;
	    $signal_num  = $? & 127;
	    $dumped_core = $? & 128;
	    if ($exit_value!=0 ||
		$signal_num!=0 ||
		$dumped_core!=0)
	    {	    
		my $nodename=$pid2node{$pid};
		my $nodecmd=$pid2cmd{$pid};
		
		$ok=0;

		if ( $nodename && $nodecmd)
		{
		    if ($self->{verbose})
		    { 
			$message->message(-1,"[".$nodename.":".$nodecmd."] exit_value:$exit_value signal_num:$signal_num dumped_core:$dumped_core");
		    }
		}
		else
		{
		    
		    if ($self->{verbose})
		    {   
			$message->message(-1,"catch pid  : $pid exit_value:$exit_value signal_num:$signal_num dumped_core:$dumped_core"); 
		    }
		    $i--;
		}
		
	    }
	}
	
    }
    return $ok;
}

sub get_status()
{
    my $self=shift;
    return $self->{status};
}

sub return_command()
{
    my $self=shift;
    return $self->{cmd};
}

1;
