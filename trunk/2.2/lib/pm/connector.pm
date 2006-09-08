package libkadeploy2::connector;
use strict;
use warnings;
use libkadeploy2::deploy_iolib;
use libkadeploy2::message;

my $message=libkadeploy2::message::new();

sub new()
{
    my $self=
    {
	nodelist  => 0,
	connector => "unknow",
    };
    bless $self;
    return $self;
}

sub set_nodelist($) { my $self=shift; my $args=shift; $self->{nodelist}=$args; }

sub get_connector()
{
    my $self=shift;
    return $self->{connector};
}


sub checkconnectors()
{
    my $self=shift;

    my $ref_node_list;
    my @node_list;
    my $node;
    my $nodename;
    my $connector="unknow";
    my $i;
    my $ok=1;
    my $nodelist;

    my $db=libkadeploy2::deploy_iolib::new();
    $db->connect();


    $nodelist=$self->{nodelist};

    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;

    $i=0;
    foreach $node (@node_list)
    {
	if ($i==0)
	{
	    $nodename=$node->get_name();
	    if ($db->get_nodestate($node->get_name(),"RSH") eq "UP")
	    { $connector="rsh"; }
	    elsif ($db->get_nodestate($node->get_name(),"SSH") eq "UP")
	    { $connector="ssh"; }
	    else
	    { 
		die "connector::checkconnectors connector:$connector...";
	    }
	    $self->{connector}=$connector;
	}
	else
	{
	    if ($db->get_nodestate($node->get_name(),uc($connector)) eq "DOWN")
	    {
		$message->message(1,$node->get_name()." $connector down");
		$ok=0;
	    }	   
	}
    }
    $db->disconnect();
    return $ok;
}



1;
