package libkadeploy2::command;

use strict;
use warnings;
use libkadeploy2::message;

my $message=libkadeploy2::message::new();

#new(command,timeout,verbose)
sub new()
{
    my $self = 
    {
	timeout     => 0,
	command     => "",
	verbose     => 0,
	openstdout  => 0,
	childpid    => -1,
	exit_value  => -1,
	signal_num  => -1,
	dumped_core => -1,
    };
    bless $self;
    return $self;
}

sub set_openstdout() { my $self=shift; $self->{openstdout}=1; }
sub set_command($)   { my $self=shift; my $args=shift; $self->{command}=$args; }
sub set_verbose()    { my $self=shift; $self->{verbose}=1; }
sub set_unverbose()  { my $self=shift; $self->{verbose}=0; }
sub set_timeout($)   { my $self=shift; my $args=shift; $self->{timeout}=$args; }


sub exec()
{
    my $self = shift;
    my $cmd = $self->{command};
    my $ok=1;
    my $timeout=$self->{timeout};
    my $pid = fork();
    
    if ($timeout<0) { $timeout=86400; }

    if (! defined($pid))
    {
	$message->message(1,"fork failed");
	$ok=0;
    }        

    if ($pid == 0)
    {
	local $SIG{ALRM} = sub { die "alarm\n" };
	alarm($timeout);
	if ($self->{verbose}) { $message->message(-1,"launch pid $pid : $cmd"); }
	
	if ($self->{openstdout}==0)
	{
	    $cmd.=" >/dev/null 2>&1 > /dev/null";
	}
	no warnings;
	exec($cmd);
	use warnings;
	$message->message(3,"malformed command line [$cmd]");
	$ok=0;
	alarm 0;
	exit 254;
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
	if ($self->get_status()) 
	{ 
	    $message->message(-1,"wait pid $pid : $cmd [OK]");
	}
	else
	{ 
	    $message->message(1,"wait pid $pid : $cmd [FAILED]"); 
	    $self->print_status();
	}
	
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

sub print_status()
{
    my $self=shift;
    $message->message(2,
		      "exit_value:".$self->{exit_value}.
		      " signal_num:".$self->{signal_num}.
		      " dumped_core:".$self->{dumped_core}
		      );
    if ($self->{signal_num}==14) 
    { $message->message(3,"Alarm catched !!!!"); }
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
