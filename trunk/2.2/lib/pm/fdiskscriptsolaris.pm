package libkadeploy2::fdiskscriptsolaris;
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

sub printfdiskdeleteprimary();
sub printfdiskformat();
sub printfdiskwrite();
sub print();

sub set_disk($)
{
    my $self=shift;
    my $disk=shift;
    $self->{disk}=$disk;
    return 1;
}

sub print()
{
    my $self=shift;
    $self->printfdiskdeleteprimary();
    $self->printfdiskformat();
    $self->printfdiskwrite();
}


sub printfdiskdeleteprimary()
{
    print "n
3
4
y
3
3
y
3
2
y
3
1
y
";
}



sub printfdiskwrite()
{
    print "5
";
}

sub sort_par_num { return $a <=> $b }

sub printfdiskformat()
{
    my $self=shift;
    my $diskref=$self->{disk};

    my $key1;
    my $key2;
    my $key3;
    my $key4;
    my $tmphash1;
    my $tmphash2;
    my $tmphash3;
    
    my $type;
    my $partnumber;
    my $size;
    my $fdisktype;
    my $disksize=0;
    
    my $ref_partitionhash=$diskref->{partition};
    my %partitionhash=%$ref_partitionhash;

    $disksize=$diskref->get_size();
    foreach my $partnumber ( sort sort_par_num keys %partitionhash)
    { 
	my $parthash=$partitionhash{$partnumber};
	my $size=$parthash->get_size();
	my $partnumber=$parthash->get_number();       
	my $fdisktype=$parthash->get_fdisktype();
	my $longtype=$parthash->get_type();
	my $ostype=$parthash->get_ostype();
	my $type="";
	my $percentsize=0;

	$size=($size/1.1); #Stupid hack for fdisk !! fuck this sh#!$ (1000 and 1024 ...)
	
	$percentsize=int($size/$disksize*100);
	
	if ($longtype eq "primary")
	{
	    if ($ostype eq "solaris")
	    {
		print "1
1
$percentsize
y
";		    
	    }
	    else
	    {
		print "1
2
$percentsize
n
";
	    }
	}		
    }
}



1;
