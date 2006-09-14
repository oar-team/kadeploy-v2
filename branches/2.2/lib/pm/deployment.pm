package libkadeploy2::deployment;
use strict;
use warning;


sub new()
{
    my $self;
    $self=
    {
	nodelist    => 0,
	environment => 0,
	state       => 0,
	startdate   => 0,
	enddate     => 0,
    };
    bless $self;
    return $self;

}

1;
