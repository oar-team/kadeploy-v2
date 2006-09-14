package libkadeploy2::errorhandler;
use strict;
use warnings;
use libkadeploy2::message;
use libkadeploy2::caller;

my $message=libkadeploy2::message::new;

sub new()
{
    my $self;
    $self=
    {
	verbose => 0,
    };
    bless $self;
    return $self;
}

sub set_verbose()                  { my $self=shift; $self->{verbose}=1; }
sub program_exit()                 { my $self=shift; exit 0;   }
sub wrong_parameters_commandline() { my $self=shift; exit 2;   }
sub unknow_error()                 { my $self=shift; exit 255; }
sub exit_failto()              
{ 
    my $self=shift; 
    my $msg=shift;
    $message->message(2,"Fail to $msg");   
    exit 1;
}

sub error_in($$)                    { 
    my $self=shift; 
    my $from=shift; 
    my $info=shift;
    my $i;

    if (! $info) { $info=""; }
    if (! $from) { $from=""; }

    $message->message(2,"(".caller()."::$from) => $info ");
    if (!$self->{verbose})
    { 	$message->message(2,"exiting (try verbose to debug)");     }
    else
    { 	$message->message(2,"exiting");     }
    
    if ($self->{verbose})
    {
    }
    exit 1; 
}



		   
1;
