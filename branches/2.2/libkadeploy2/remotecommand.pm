package libkadeploy2::remotecommand;

use strict;
use warnings;

sub new($$$$$$)
{
    my $connector=shift;
    my $login=shift;
    my $node=shift;
    my $cmd=shift;
    my $timeout=shift;
    my $verbose=shift;
    my $self=
    {
	connector        => $connector,
	login            => $login,
	node             => $node,
	cmd              => $cmd,
	timeout          => $timeout,
	verbose          => $verbose,
	status           => -1,
	};
    bless $self;
    return $self;
}

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
	print STDERR "ERROR : connector not found : ".$self->{connector}."\n";
	exit 1;
    }	
}

sub exec_rsh()
{
    my $self=shift;
    my $cmd;
    my $command;
    $cmd="rsh -l ".$self->{login}." ".$self->{node}->get_ip()." ".$self->{cmd};

    $command=libkadeploy2::command::new($cmd,
					$self->{timeout},
					$self->{verbose}
					);

    $self->{status}=$command->exec();
    return($self->{status});
}

sub exec_ssh()
{
    my $self=shift;
    my $cmd;
    my $command;
    $cmd="ssh -l ".$self->{login}." ".$self->{node}->get_ip()." ".$self->{cmd};

    $command=libkadeploy2::command::new($cmd,
					$self->{timeout},
					$self->{verbose}
					);
    $self->{status}=$command->exec();
    return($self->{status});
}

sub get_status()
{
    my $self=shift;
    return $self->{status};
}

1;
