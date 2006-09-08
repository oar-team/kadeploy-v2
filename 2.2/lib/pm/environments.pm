package libkadeploy2::environments;

use strict;
use warnings;
use libkadeploy2::deploy_iolib;
use libkadeploy2::message;

my $message=libkadeploy2::message::new();

sub new()
{
    my $self=
    {
	ENV => 0,
    };
    bless $self;
    return $self;
}

sub get()
{
    my $self=shift;
    my $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    $self->{ENV}=$db->get_environments();
    $db->disconnect();    
}

sub print_header()
{
    my $self=shift;
    my $environment=libkadeploy2::environment::new();
    $environment->print_header(1);
}

sub print_footer()
{
    my $self=shift;
    my $environment=libkadeploy2::environment::new();
    $environment->print_footer(1);
}


sub print()
{
    my $self=shift;
    my $filter=shift;
    my $ref_array;
    my @array;
    my $line;
    my $refline;
    my $i=0;
    my $environment;
    my $name;
    my $user;
    my $oldstyle=1;
    my $descriptionfile;

    $ref_array=$self->{ENV};
    if (! $ref_array) { $message->missing_env_db(0); return 1; }
    foreach $refline (@$ref_array)
    {
	$name=$$refline[0];
	$user=$$refline[1];
	$descriptionfile=$$refline[2];
	
	$environment=libkadeploy2::environment::new();
	$environment->set_name($name);
	$environment->set_user($user);
	$environment->set_descriptionfile($descriptionfile);
	

	if (! $filter)
	{
	    $environment->print_line($oldstyle);
	}
	elsif ($filter eq $name ||
	    $filter eq $user)
	{
	    $environment->print_line($oldstyle);
	}
    }    
}



