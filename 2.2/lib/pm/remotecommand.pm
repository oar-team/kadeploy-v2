package libkadeploy2::remotecommand;

use libkadeploy2::deployconf;
use libkadeploy2::message;
use strict;
use warnings;

my $message=libkadeploy2::message::new();
my $conf=libkadeploy2::deployconf::new();
$conf->load();

sub new()
{
    my $self=
    {
	connector        => "ssh",
	login            => "",
	node             => 0,
	command          => "",
	timeout          => 0,
	verbose          => 0,
	status           => -1,

	openstdout       => 0,
    };
    bless $self;
    return $self;
}

sub set_openstdout() { my $self=shift; $self->{openstdout}=1; }
sub set_command($)   { my $self=shift; my $args=shift; $self->{command}=$args; }
sub set_verbose()    { my $self=shift; $self->{verbose}=1; }
sub set_unverbose()  { my $self=shift; $self->{verbose}=0; }
sub set_timeout($)   { my $self=shift; my $args=shift; $self->{timeout}=$args; }

sub set_login($)     { my $self=shift; my $args=shift; $self->{login}=$args; }
sub set_connector($) { my $self=shift; my $args=shift; $self->{connector}=$args; }
sub set_node($)      { my $self=shift; my $args=shift; $self->{node}=$args; }


sub exec()
{
    my $self=shift;
    my $findconnector=0;
    if ($self->{connector} eq "rsh")
    {
	return $self->exec_rsh();
	$findconnector=1;
    }
    if ($self->{connector} eq "ssh")
    {
	return $self->exec_ssh();
	$findconnector=1;
    }
    if ($findconnector==0)
    {
	$message->message(2,"INTERNAL connector not found : ".$self->{connector});
	exit 1;
    }	
}

sub exec_rsh()
{
    my $self=shift;
    my $cmd;
    my $command;

    $cmd="rsh -l ".$self->{login}." ".$self->{node}->get_ip()." ".$self->{command};    
    return $self->exec_prot($cmd);
}

sub exec_ssh()
{
    my $self=shift;
    my $cmd;
    my $command;
    my $ssh_default_args;

    $ssh_default_args=$conf->get("ssh_default_args");
    if (! $ssh_default_args) { $ssh_default_args=""; }
    $cmd="ssh ".$ssh_default_args." -l ".$self->{login}." ".$self->{node}->get_ip()." ".$self->{command};
    return $self->exec_prot($cmd);
}


sub exec_prot($)
{
    my $self=shift;
    my $cmd=shift;
    my $command;

    $command=libkadeploy2::command::new();
    $command->set_command($cmd);
    $command->set_timeout($self->{timeout});
    if ($self->{verbose})    { $command->set_verbose(); }
    if ($self->{openstdout}) { $command->set_openstdout(); }
	
    $self->{status}=$command->exec();
    return($self->{status});
}
    

sub get_status()
{
    my $self=shift;
    return $self->{status};
}

1;
