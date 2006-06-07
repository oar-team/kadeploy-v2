package libkadeploy2::command;

use strict;
use warnings;


#new(command,timeout,verbose)
sub new($$$)
{
    my $command=shift;
    my $timeout=shift;
    my $verbose=shift;
    my $self = 
    {
	timeout     => $timeout,
	command     => $command,
	verbose     => $verbose,
	childpid    => -1,
	exit_value  => -1,
	signal_num  => -1,
	dumped_core => -1,
    };
    bless $self;
    return $self;
}

sub exec()
{
    my $self = shift;
    my $cmd = $self->{command};
    my $ok=1;
    my $pid = fork();
    if (! defined($pid))
    {
	warn("WARNING : fork system call failed\n");
	$ok=0;
    }        

    if ($pid == 0)
    {
	local $SIG{ALRM} = sub { die "alarm\n" };       # NB \n required
	alarm($self->{timeout});
	if ($self->{verbose}==1) { print("cmd_launch: $cmd \n"); }
	if ($self->{verbose}>=2)
	{
	    exec($cmd);
	}
	
	if ($self->{verbose}==1 ||
	    $self->{verbose}==0
	    )
	{
#	    close STDOUT;
#	    close STDERR;		
	    exec($cmd." 2>&1 > /dev/null");
	}
	$ok=0;
	alarm 0;
	exit 1;
    }
    else
    {
	$self->{waited_pid}  = wait();
	$self->{exit_value}  = $? >> 8;
	$self->{signal_num}  = $? & 127;
	$self->{dumped_core} = $? & 128;
    } 
    if ($self->{verbose}) 
    { 
	print "cmd_return: $cmd ";
	if ($self->get_status()==1) { print " [OK]\n"; }
	else                        { print " [FAILED]\n"; }
    }
    return $self->get_status();
}

sub get_status()
{
    my $self=shift;
    my $ok=0;
    if (
	$self->{exit_value}==0 &&
	$self->{signal_num}==0 &&
	$self->{dumped_core}==0
	)
    {
	$ok=1;
    }
    else
    {
	$ok=0;
    }
    return $ok;
}

sub get_exit_value()
{
    my $self=shift;
    return $self->{exit_value};
}

sub get_signal_num()
{
    my $self=shift;
    return $self->{signal_num};
}

sub get_dumped_core()
{
    my $self=shift;
    return $self->{dumped_core};
}

sub return_command()
{
    my $self=shift;
    return $self->{command};
}

1;
