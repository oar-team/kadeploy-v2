package libkadeploy2::checknodes;

use libkadeploy2::remotecommand;
use libkadeploy2::message;
use libkadeploy2::node;
use warnings;
use strict;

my $message=libkadeploy2::message::new();

sub new($$)
{
    my $node=shift;
    my $db=shift;
    my $self;
    $self=
    {
	db   => $db,
	node => $node,
    };
    bless $self;
    return $self;
}


sub exec()
{
    my $self=shift;
    my $node=$self->{node};
    my $db=$self->{db};
    $message->message(0,"Checking ".$node->get_name()." ip and services");
    $self->check_node_icmp();
    $self->check_node_ssh();
    $self->check_node_mcat();
}

sub check_node_icmp()
{
    my $self=shift;
    my $node=$self->{node};
    my $db=$self->{db};
    
    my $nmap=libkadeploy2::nmap::new();
    if ($nmap->check_icmp($node->get_ip()))
    {
	$db->update_nodestate($node->get_name(),"ICMP","UP");
    }
    else
    {
	$db->update_nodestate($node->get_name(),"ICMP","DOWN");
    }
    
}

sub check_node_ssh()
{
    my $self=shift;
    my $node=$self->{node};
    my $db=$self->{db};

    my $nmap=libkadeploy2::nmap::new();
    if ($nmap->check_tcp($node->get_ip(),22))
    {
	$db->update_nodestate($node->get_name(),"SSH","UP");
    }
    else
    {
	$db->update_nodestate($node->get_name(),"SSH","DOWN");
    }   

}


sub check_node_mcat()
{
    my $self=shift;
    my $node=$self->{node};
    my $db=$self->{db};
    my $ok;
    my $remotecommand=libkadeploy2::remotecommand::new("ssh",
						       "root",
						       $node,
						       "which mcatseg",
						       10,
						       0
						       );
    $remotecommand->exec();
    if ($remotecommand->get_status())     
    {  	
	$ok=1;    
	$db->update_nodestate($node->get_name(),"MCAT","UP");
	$remotecommand=libkadeploy2::remotecommand::new("ssh",
							"root",
							$node,
							"pkill mcatseg",
							10,
							0
							);
	$remotecommand->exec();
	if (! $remotecommand->get_status())
	{ 	$ok=0;     }
	return $ok;
    }
    else
    {
	$db->update_nodestate($node->get_name(),"MCAT","DOWN");
    }
}
    
