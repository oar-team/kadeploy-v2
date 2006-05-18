package libkadeploy2::nmap;
use strict;
use warnings;


sub new()
{
    my $self;
    $self=
    {
    };
    bless $self;
    return $self;
}

sub check_tcp($$)
{
    my $self = shift;

    my $hostip=shift;
    my $port=shift;

    my $line;
    my $ok=0;

    open(NMAPCMD, "nmap -p $port -oG - $hostip|");
    while ($line=<NMAPCMD>)
    {
	if ($line=~ /^Host: $hostip \(.+\)[\s\t]+Ports: $port\/open\//)
	{
	    $ok=1;
	}
    }
    close(NMAPCMD);
    return $ok;
}

sub check_icmp($)
{
    my $self = shift;

    my $hostip=shift;

    my $line;
    my $ok=0;

    open(NMAPCMD, "nmap -sP -oG - $hostip|");
    while ($line=<NMAPCMD>)
    {
	if ($line=~ /^Host: $hostip \(.+\)[\s\t]+Status:[\s]Up/)
	{
	    $ok=1;
	}
    }
    close(NMAPCMD);
    return $ok;
}


1;
