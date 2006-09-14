package libkadeploy2::kamcat;

use Getopt::Long;
use libkadeploy2::command;
use libkadeploy2::remotecommand;
use libkadeploy2::node;
use libkadeploy2::nodelist;
use libkadeploy2::remoteparallelcommand;
use libkadeploy2::deploy_iolib;
use libkadeploy2::message;
use libkadeploy2::tools;
use libkadeploy2::remotecopy;
use libkadeploy2::connector;
use POSIX;
use warnings;
use strict;

sub passive_check_nodes();
sub setup_nodes();
sub setup_cmdnode();
sub get_options_cmdline();
sub set_nodelist($);
sub set_port($);    
sub set_srvcmd($);
sub set_cltcmd($);
sub set_user($);

my $conf=libkadeploy2::deployconf::new();
if (! $conf->load()) { exit 1; }
my $message=libkadeploy2::message::new(); 

my $cmdnode;
my $fifo="/tmp/mcatfifo";
my $defaultport=10000;
my $port;
my $srvcmd;
my $cltcmd;
my $user;
my $help;
my $remotepath="PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin";
################################################################################

sub new()
{
    my $connector=libkadeploy2::connector::new();
    my $self=
    {
	help      => 0,
	nodelist  => 0,
	verbose   => 0,
	timeout   => 0,
	connector => $connector
    };

    bless $self;
    return $self;
}


sub set_nodelist($) {  my $self=shift; $self->{nodelist}=shift; }
sub set_port($)     {  my $self=shift; $self->{port}=shift; }
sub set_srvcmd($)   {  my $self=shift; $self->{srvcmd}=shift; }
sub set_cltcmd($)   {  my $self=shift; $self->{cltcmd}=shift; }
sub set_user($)     {  my $self=shift; $self->{user}=shift;  }
sub set_verbose($)  {  my $self=shift; $self->{verbose}=shift; }
sub set_timeout($)  {  my $self=shift; $self->{timeout}=shift; }

sub get_options_cmdline()
{
    my $self=shift;
    my @node_list;
    my $verbose=0;
    GetOptions(
	       'm=s'                  => \@node_list,
	       'machine=s'            => \@node_list,
	       'port=s'               => \$port,
	       'p=s'                  => \$port,
	       'servercommand=s'      => \$srvcmd,
	       'sc=s'                 => \$srvcmd,
	       'clientcommand=s'      => \$cltcmd,
	       'dc=s'                 => \$cltcmd,
	       'login=s'              => \$user,
	       'l=s'                  => \$user,
	       'h!'                   => \$help,
	       'help!'                => \$help,
	       'v!'                   => \$verbose,
	       'verbose!'             => \$verbose,
	       );
    $self->{port}=$port;
    $self->{srvcmd}=$srvcmd;
    $self->{cltcmd}=$cltcmd;
    $self->{user}=$user;
    $self->{help}=$help;
    if ($verbose) { $self->{verbose}=$verbose+2; }

    if (@node_list)    
    { 
	$self->{nodelist}=libkadeploy2::nodelist::new(); 
	$self->{nodelist}->loadlist(\@node_list);	
    }
}

sub check_option()
{
    my $self=shift;

    if ($self->{help})        { $message->kamcat_help(); return 0; }
    if (! $self->{port})      { $self->{port}=getpid(); 
				if ($self->{port} < 1024) { $self->{port}+=10000; } }


    if (! $self->{user})      { $self->{user}="root"; }


    if (! $self->{srvcmd})    { $message->kamcat_help(); return 0; }
    if (! $self->{cltcmd})    { $message->kamcat_help(); return 0; }   
    elsif (!$self->{nodelist})
    {
	$message->missing_node_cmdline(2);
	return 0;
    }

    return 1;
}

sub run()
{
    my $self=shift;
    my $connector=$self->{connector};
    if (! $self->check_option()) 
    { 
	$message->message(2,"check_option failed (kamcat.pm)");
	return 1; 
    }

    if ($self->{nodelist})
    {
	$connector->set_nodelist($self->{nodelist});
	if (!$connector->checkconnectors()) { $message->message(2,"check connectors... failed\n"); return 1; }
	$self->{connector}=$connector;

	if (! $self->passive_check_nodes()) { $message->message(2,"check node... failed\n"); return 1;}
	if (! $self->setup_nodes())         { $message->message(2,"setup node... failed\n"); return 1;}
	if ($self->setup_cmdnode())         { return 0;	} 
	else 	{ return 1; }	
    }
}


sub passive_check_nodes()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $node;
    my $ok=1;
    my $ref_node_list;
    my @node_list;
    my $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    foreach $node (@node_list)
    {
	if (! ($db->get_nodestate($node->get_name(),"MCAT") eq "UP"))
	{
	    $message->message(2,"mcat service for node ".$node->get_name()." is not up");
	    $ok=0;
	}
    }
    $db->disconnect();
    return $ok;
}


sub setup_nodes()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $ok=1;
    my $node;
    my @cmdlist;
    my $refcmdlist;
    my $ref_node_list;
    my $cmd;
    my $parallelcommand;
    my $ssh_default_args;
    my @node_list;
    my $verbose=$self->{verbose};
    my $hostfileinstr=libkadeploy2::tools::returnvalidhostsfile();
    my $connector=$self->{connector};


    my $remotecopy=libkadeploy2::remotecopy::new();
    if ($connector->get_connector() eq "ssh")
    {
	$remotecopy->set_connector($connector->get_connector());
	$remotecopy->set_login("root");
	$remotecopy->set_nodelist($nodelist);
	$remotecopy->set_sourcefile("/etc/hosts");
	$remotecopy->set_destinationfile("/etc/hosts");
	$remotecopy->set_timeout(10);
	if ($self->{verbose}) { $remotecopy->set_verbose(); }
	$remotecopy->exec();
    
	if ($remotecopy->get_status())  { $ok=1; }
	else     { 	$ok=0;     }
    }

    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;

    $ssh_default_args=$conf->get("ssh_default_args");
    if (! $ssh_default_args) { $ssh_default_args=""; }


    foreach $node (@node_list)
    {
	if ($connector->get_connector() eq "ssh")
	{
	    $cmd="ssh ".$ssh_default_args." -l root ".$node->get_ip()." hostname ".$node->get_name();
	}
	elsif ($connector->get_connector() eq "rsh")
	{
	    $cmd="rsh -l root ".$node->get_ip()." hostname ".$node->get_name();
	}
	@cmdlist=(@cmdlist,$cmd);	
    }    

    $refcmdlist=\@cmdlist;
    $parallelcommand=libkadeploy2::parallelcommand::new();
    $parallelcommand->set_timeout(10);
    if ($self->{verbose}>1) { $parallelcommand->set_verbose(); }
    if (! $parallelcommand->execparallel($refcmdlist)) { $ok=0; }
    return $ok;
}

sub setup_cmdnode()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my @node_list;
    my $ref_node_list;
    my $ok;
    my @cmdlist;
    my $cmd;
    my $node;
    my $i;
    my $forknumber;
    my $sleeptime=20;
    my $timeout=$self->{timeout};
    my $command;
    my $ssh_default_args;
    my $connector=$self->{connector};


    if (! $timeout) { $timeout=7200; }
    #Never the same port (hugly....)
    my $pid=fork();
    if ($pid==0) { exit 0; }
    wait();
    $self->{port}=$pid;

    #Activate pipe viewer if pv=yes in deploy.conf
    if ($conf->is_conf("pv"))
    { 
	if ($conf->get("pv") =~ /yes/) 
	{
	    $self->{srvcmd}=~s/^cat/pv -N mcat/;
	}
    }
	

    $ref_node_list=$nodelist->get_ip_list();
    @node_list=@$ref_node_list;

    $ssh_default_args=$conf->get("ssh_default_args");
    if (! $ssh_default_args) { $ssh_default_args=""; }
    if ($self->{connector} eq "ssh") 
    { $connector="ssh ".$ssh_default_args; }
    else { $connector=$self->{connector};   }

    for ($i=0; $i< $#node_list+1;$i++)
    {

	#Seeking mcatseg with $PATH on the node
	# 1 is for receiver
	my $prefixcmd=$connector->get_connector();
	if ($prefixcmd =~ /ssh/)
	{ $prefixcmd.=" $ssh_default_args"; }

	$cmd="$prefixcmd ".
	    " -l ".$self->{user}." ".$node_list[$i]." ".
	    "\" mcatseg 1 ".
	    $self->{port}." ".
	    "\\\""."ls"."\\\" ".
	    "\\\"".$self->{cltcmd}."\\\" ".
	    "@node_list \"";
	@cmdlist=(@cmdlist,$cmd);
    }       
    if ($i>0)
    {

	#mcatseg is in path
	# 4 is for sender
	$message->message(-1,"srv cmd: [".$self->{srvcmd}."] node cmd: [".$self->{cltcmd}."]");
	$cmd=$conf->getpath_cmd("mcatseg")." 4 ".$self->{port}.
	    " \"".$self->{srvcmd}." \" ".
	    " \"".$self->{cltcmd}." \" ".
	    $node_list[0];	
	@cmdlist=(@cmdlist,$cmd);
    }



#    foreach my $tmp (@cmdlist) { print "$tmp\n"; }
    $i=0;
    $forknumber=0;
    for ($i=0; $i<$#node_list+1;$i++)
    {
	$cmd=$cmdlist[$i];
	if (fork()==0)
	{
	    exec($cmd. " >/dev/null 2>&1 > /dev/null");
	}
	else
	{
	    if ($self->{verbose}==2)
	    { 	$message->message(-1,"exec cmd $cmd"); }
	    $forknumber++;
	}
    }
    $cmd=$cmdlist[$i];
    sleep(2);
    
    

    $command=libkadeploy2::command::new();
    $command->set_command($cmd);
    $command->set_timeout($timeout);

    $command->set_openstdout();
    $command->exec();
    for ($i=1;$i<$forknumber;$i++) { wait(); }
    return $command->get_status();
}


1;
