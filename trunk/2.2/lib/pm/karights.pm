package libkadeploy2::karights;
use strict;
use warnings;
use libkadeploy2::deployconf;
use libkadeploy2::deploy_iolib;
use Getopt::Long;
use libkadeploy2::deployconf;
use libkadeploy2::nodelist;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::right;
use libkadeploy2::rights;
use libkadeploy2::cmdline;
use libkadeploy2::karights;

my @hostlist;
my $host;
my $hostfile;
my $nodelist;
my $node;
my $add;
my $del;
my $login;
my $listnode;
my $help;
my @rights;
my $right;
my $rights;
my $list;
my $check;
my $privileged_user;
my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();
my $conf=libkadeploy2::deployconf::new();  
if ($conf->is_conf("privileged_user")) 
{ $privileged_user=$conf->get("privileged_user"); }    
$conf->load();

sub new()
{
    my $self=
    {
	add => 0,
	del => 0,
	list => 0,
	check => 0,

	nodelist => 0,
    };
    bless $self;
    return $self;
}

sub set_nodelist($)     { my $self=shift; my $arg=shift; $self->{nodelist}=$arg;    }

sub run()
{
    my $self=shift;
    my $add=$self->{add};
    my $del=$self->{del};
    my $list=$self->{list};
    my $check=$self->{check};
    my $ref_rights=$self->{rights};
    my @rights=@$ref_rights;

    if (! $self->check_options())
    { 
	$message->message(2,"wrong parameters commandline");
	$errorhandler->wrong_parameters_commandline();
    }



    if ( (! $add) &&
	 (! $del) &&
	 (! $list) &&
	 (! $check)
	 )
    {
	$message->missing_cmdline(2,"flags");
	exit 1;
    }
    
    if ($list)
    {
	if (@hostlist || $hostfile)	
	{
	    my $reflist;
	    if ($hostfile) 
	    { 
		$nodelist=libkadeploy2::cmdline::loadhostfileifexist($hostfile); 
	    }
	    if (@hostlist) 
	    { 
		$nodelist=libkadeploy2::cmdline::loadhostcmdlineifexist(\@hostlist);
	    }
	    
	    $reflist=$nodelist->get_nodes();
	    foreach $node (@$reflist)
	    {
		$rights=libkadeploy2::rights::new();	   
		$rights->get();
		$rights->print_node($node->get_name());				
	    }
	    exit 0;
	}
	elsif ((! $login) && (! @rights))
	{
	    $rights=libkadeploy2::rights::new();
	    $rights->get();
	    $rights->print();
	    exit 0;
	}
	elsif ($login)
	{
	    $rights=libkadeploy2::rights::new();
	    $rights->get();
	    $rights->print_user($login);
	    exit 0;
	}
	elsif (@rights)
	{
	    foreach $right (@rights)
	    {
		$rights=libkadeploy2::rights::new();	   
		$rights->get();
		$rights->print_rights($right);				
	    }
	    exit 0;
	}
    }
    
    
    
    if ( (! @hostlist ) && (! $hostfile) )
    {
	$message->missing_cmdline(2,"node");
	$message->karights_help();
	exit 0;
    }
    
    if ($hostfile) 
    { 
	$nodelist=libkadeploy2::cmdline::loadhostfileifexist($hostfile); 
    }
    if (@hostlist) 
    { 
	$nodelist=libkadeploy2::cmdline::loadhostcmdlineifexist(\@hostlist); 
    }
    
    
    
    if (! $login)
    {
	$message->missing_cmdline(2,"login");
	exit 1;
    }
    
    if (! @rights)
    {
	$message->missing_cmdline(2,"rights");
	exit 1;
    }
    
    
    if ($add)
    {
	my $retcode=0;
	my $reflist=$nodelist->get_nodes();
	foreach $node (@$reflist)
	{
	    foreach my $righttoadd (@rights)
	    {
		$right=libkadeploy2::right::new();
		$right->set_user($login);
		$right->set_right($righttoadd);
		$right->set_node($node->get_name());
		if ($right->addtodb())
		{
		    $message->message(0,"Add $righttoadd to $login@".$node->get_name());
		}
		else
		{
		    $message->message(2,"Fail to add $righttoadd to $login ".$node->get_name());
		    $retcode=1;
		}
	    }
	}
	exit $retcode;
    }
    
    if ($del)
    {
	my $retcode=0;
	my $reflist=$nodelist->get_nodes();
	foreach $node (@$reflist)
	{
	    foreach my $righttodel (@rights)
	    {
		$right=libkadeploy2::right::new();
		$right->set_user($login);
		$right->set_right($righttodel);
		$right->set_node($node->get_name());
		if ($right->delfromdb())
		{
		    $message->message(0,"Remove $righttodel to user $login@".$node->get_name());
		}
		else
		{
		    $message->message(2,"Fail to del $righttodel to $login ".$node->get_name());
		    $retcode=1;
		}
	    }
	}
	exit $retcode;
    }
    
    if ($check)
    {
	my $ok=1;
	my $i;
	my $db=libkadeploy2::deploy_iolib::new();
	$db->connect();
	
	$rights=libkadeploy2::rights::new();
	
	$rights->get();
	
	foreach $right (@rights)
	{
	    my $reflist=$nodelist->get_nodes();
	    foreach $node (@$reflist)
	    {
		if (! libkadeploy2::karights::check($login,$node,$right)) { $ok=0; }
	    }
	}
	$db->disconnect();
	if ($ok) { exit 0; }
	else     { exit 1; }
    }    
}

sub get_options_cmdline()
{
    my $self=shift;
    my $getopt=0;
    my @node_list=();
    my $nodelist;
    my $envname="";
    my $help;
    my @rights;
    my $verbose=0;
    my $ref_rights;
    $getopt=GetOptions(
		       'add!'           => \$add,
		       'del!'           => \$del,
		       'list!'          => \$list,
		       'check!'         => \$check,
		       
		       
		       'h!'             => \$help,
		       'help!'          => \$help,
		       
		       'm=s'            => \@hostlist,
		       'machine=s'      => \@hostlist,
		       
		       'f=s'            => \$hostfile,
		       'nodefile=s'     => \$hostfile,
		       
		       'r=s'            => \@rights,
		       'rights=s'       => \@rights,
		       
		       'l=s'            => \$login,
		       'verbose!'       => \$verbose,
		       'v!'             => \$verbose,
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
    $ref_rights=\@rights;

    $self->{add}=$add;
    $self->{del}=$del;
    $self->{list}=$list;
    $self->{check}=$check;
    $self->{help}=$help;
    $self->{login}=$login;
    $self->{rights}=$ref_rights;
    $self->{verbose}=$verbose;
}

sub check_options()
{
    my $self=shift;
    my $ok=1;
    if ($self->{help}) { $message->karights_help(); exit 0;  }
    return $ok;
}

sub check_db($$$)
{
    my $self=shift;
    my $user=shift;
    my $node=shift;
    my $right=shift;

    my $i;
    my $rights;
    my $db;
    my $ok=1;

    $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    if (! $db->check_rights($user,$node->get_name(),$right)) { $ok=0; }
    $db->disconnect();
    return $ok;
}

sub check_rights($)
{
    my $self=shift;
    my $right=shift;

    my $nodelist=$self->{nodelist};
    my $ref_node_list;
    my @node_list;
    my $ok=0;
    my $count=0;
    my $listok=0;
    my $sudo_user;
    my $node;
    my $privileged_user="";

    $sudo_user=libkadeploy2::sudo::get_sudo_user();
    if (! $sudo_user) 
    { $sudo_user=libkadeploy2::sudo::get_user(); }
    if ( $sudo_user eq "root" ||
	 $sudo_user eq $conf->get("deploy_user") ||
	 $sudo_user eq $privileged_user
	 )
    {
	$ok=1;
    }
    
    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    foreach $node (@node_list)
    {
	$count++;
	if ($self->check_db($sudo_user,
			    $node,
			    $right))
	{
	    $listok++;
	}
    }    
    if (! $ok) { if ($count==$listok && $count!=0) { $ok=1; } }
    return $ok;
}



1;
