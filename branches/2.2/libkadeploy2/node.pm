## manage node relative content
package libkadeploy2::node;

use strict;
use warnings;
use libkadeploy2::deploy_iolib;
#my $default_state = 0;

## Class constructor
# 
# check all the relevant data available for the node
#
# args: 
#
# must call set_IP to set the node IP after that!

sub new()
{
    my $self = {};
    $self->{name} = "[NOTSET]";
    $self->{ip}="[NOTSET]";
    $self->{mac}="[NOTSET]";
    $self->{state}="[CREATED]";
    bless $self;
    return $self;
}


## state
# CREATED: initial state, know nothing about the node

sub get_state()
{
    my $self = shift;
    return $self->{state};
}

sub set_state($)
{
    my $self = shift;
    $self->{state} = shift;
}



## name
# get name of the node
sub get_name()
{
    my $self = shift;
    if ($self->{name} eq "[NOTSET]")
    {
	print STDERR "ERROR : node::get_name() ip is not set...\n";
	exit 1;
    }    
    return $self->{name};
}

sub set_name($)
{
    my $node=shift;
    $node->{name}=shift;
}

## IP
# IP of the node
sub set_ip($)
{
    my $self = shift;
    my $ip=shift;
    if ($ip=~/^\d+\.\d+\.\d+\.\d+$/)
    {
	$self->{ip} = $ip;
    }
    else
    {
	print STDERR "ERROR : node::set_ip(ip) wrong ip\n";
	exit 1;
    }
}

sub get_ip()
{
    my $node = shift;
    my $ok;
    if ($node->{ip} eq "[NOTSET]")
    {
	print STDERR "ERROR : node::get_ip() ip is not set...\n";
	exit 1;
    }
    return $node->{ip};
}

sub set_mac($)
{
    my $self=shift;
    my $mac=shift;
    my $ok=0;
    if ($mac=~/..:..:..:..:..:../)
    {
	$self->{mac}=$mac;
	$ok=1;
    }
    else
    {
	$ok=0;
    }
    return $ok;
}

sub get_mac($)
{
    my $self=shift;
    my $mac=shift;
    if ($self->{mac} eq "[NOTSET]")
    {
	print STDERR "ERROR : node::get_mac() mac is not set...\n";
	exit 1;
    }
    return $self->{mac};
}

sub print()
{
    my $self=shift;
    print $self->get_name()." ".$self->get_mac()." ".$self->get_ip()."\n";
}

sub loadfile($)
{
    my $self=shift;
    my $file=shift;
    my $line;
    my $name;
    my $mac;
    my $ip;
    my $ok=0;

    open(DESC,$file) or die "Can't open $file\n";
    foreach $line (<DESC>)
    {
	chomp($line);
	if($line)
	{
	    #check line 
	    #node1.cluster.net 11:22:33:44:55:66 192.168.0.1
	    if($line =~ /^([a-z0-9\-\.]+)[\t\s]+(..:..:..:..:..:..)[\t\s]+([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)[\t\s]*$/)
	    {
		($name,$mac,$ip)=($1,$2,$3);
		$self->set_name($name);
		$self->set_mac($mac);
		$self->set_ip($ip);
		$ok=1;
	    }
	}
    }
    return $ok;
}

sub addtodb()
{
    my $self=shift;
    my @info;
    my $name;
    my $mac;
    my $ip;
    my $db;    
    my $node_id;
    my $ok=0;

    $db = libkadeploy2::deploy_iolib::new();
    $db->connect();
    $name=$self->get_name();    
    $ip=$self->get_ip();
    $mac=$self->get_mac();
    @info = ($name,$mac,$ip);
    $node_id = $db->add_node(\@info);
    $db->disconnect();
    if ($node_id) { $ok=1; }
    return $ok;
}

sub getfromdb($) #(nodename)
{
    my $self=shift;
    my $name=shift;
    my $db;
    my $nodeid;
    my $ok;

    $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    $nodeid=$db->node_name_to_id($name);
    if ($nodeid)
    {
	$self->set_name($db->node_id_to_name($nodeid));
	$self->set_ip($db->node_id_to_ip($nodeid));
	$self->set_mac($db->node_id_to_mac($nodeid));
	$ok=1;
    }
    else
    {
	$ok=0;
    }
    return $ok;
}

1;
