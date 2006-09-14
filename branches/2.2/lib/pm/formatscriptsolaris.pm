package libkadeploy2::formatscriptsolaris;
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

sub printformatdeleteprimary();
sub printformatformat();
sub printformatwrite();
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
    $self->printformatdeleteprimary();
    $self->printformatformat();
    $self->printformatwrite();
}


sub printformatdeleteprimary()
{
    print "0
partition
0
unassigned
wm

0
1
unassigned
wm

0
2
unassigned
wm

0
3
unassigned
wm

0
4
unassigned
wm

0
5
unassigned
wm

0
6
unassigned
wm

0
7
unassigned
wm

0
";
}



sub printformatwrite()
{
    print "label
y
quit
quit
";
}

sub sort_par_num { return $a <=> $b }

sub printformatformat()
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
    my $slicenumber=0;
    
    my $ref_partitionhash=$diskref->{partition};
    my %partitionhash=%$ref_partitionhash;

    foreach my $partnumber ( sort sort_par_num keys %partitionhash)
    { 
	my $parthash=$partitionhash{$partnumber};
	my $size=$parthash->get_size();
	my $partnumber=$parthash->get_number();       
	my $fdisktype=$parthash->get_fdisktype();
	my $longtype=$parthash->get_type();
	my $ostype=$parthash->get_ostype();
	my $label=$parthash->get_label();
	my $type="";
	my $percentsize=0;

	$size=int($size/1.1); #Stupid hack for fdisk !! fuck this sh#!$ (1000 and 1024 ...)
	
	
	if ($longtype eq "slice")
	{
	    print "$slicenumber
$label
wm

$size mb
";		
	    $slicenumber++;
	}
    }
}



1;
