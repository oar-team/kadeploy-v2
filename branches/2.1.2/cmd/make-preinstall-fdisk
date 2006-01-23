#!/usr/bin/perl

use strict;
use warnings;

sub loadpartitionfile();
sub printpart();
sub printfdiskformat();
sub printfdiskdeleteprimary();
sub printfdiskwrite();
sub checkcorrectness();

my $partitionfile="/etc/kadeploy/clusterpartition.conf";
my %disk;
my %partitions;
$disk{partition}=\%partitions;

loadpartitionfile();
checkcorrectness();
#printpart();
printfdiskdeleteprimary();
printfdiskformat();
printfdiskwrite();

################################################################################

sub checkcorrectness()
{
    my $key1;
    my $key2;
    my $key3;
    my $key4;
    my $tmphash1;
    my $tmphash2;
    my $tmphash3;
    
    my $numberofprimarypart=0;
    my $numberofextendedpart=0;
    my $numberoflogicalpart=0;
    my $sizeoflogical=0;
    my $sizeofprimary=0;
    my $sizeofdisk=0;
    my $actualpartnumber=0;
    #disk 
    foreach $key1 (sort keys %disk)
    {
	$tmphash1 = $disk{$key1};
	if (ref($tmphash1) eq 'HASH')
	{
	    #partition
	    foreach $key2 (sort keys %$tmphash1)
	    {
		$tmphash2 = $$tmphash1{$key2};		
		$actualpartnumber=$key2;
		if (ref($tmphash2) eq 'HASH')
		{
		    #attribute of partition
		    foreach $key3 (sort keys %$tmphash2) 
		    {
			if ($$tmphash2{$key3} eq "primary")
			{
			    $numberofprimarypart++;			    
			}
			if ($$tmphash2{$key3} eq "extended")
			{
			    $numberofextendedpart++;
			}
			if ($$tmphash2{$key3} eq "logical")
			{
			    $numberoflogicalpart++;
			}
			if ($actualpartnumber<5 &&
			    ($key3 eq "size")
			    )
			    
			{
			    $sizeofprimary+=$$tmphash2{$key3};
			}
			if ($actualpartnumber>=5 &&
			    ($key3 eq "size")			    
			    )
			{
			    $sizeoflogical+=$$tmphash2{$key3};
			}			
		    }
		}
	    }
	}
	else
	{
	    if ($key1 eq "size")
	    {
		$sizeofdisk=$disk{$key1};
	    }
	}
    }
    if ($numberofprimarypart + $numberofextendedpart > 4)
    {
	print STDERR "Error too much primary or extended partition.\n";
	exit 1;
    }
    if ($sizeofdisk<$sizeofprimary ||
	$sizeofdisk<$sizeoflogical)
    {
	print STDERR "Error size of disk < size of partition\n";
	exit 1;
    }

print STDERR "primary part                  : $numberofprimarypart\n";
print STDERR "extended part                 : $numberofextendedpart\n";
print STDERR "logical part                  : $numberoflogicalpart\n";
print STDERR "size of disk                  : $sizeofdisk\n";    
print STDERR "size of primary and extended  : $sizeofprimary\n";    
print STDERR "size of logical               : $sizeoflogical\n";    

}

sub loadpartitionfile()
{
    my $line;
    my ($number,$size,$fdisktype,$label,$type);
    my $linecorrect=0;
    $type="";

    if (open(FH,$partitionfile))
    {
	foreach $line (<FH>)
	{
	    my %partition;
	    if($line =~ /^(hd[a-z]) size=([0-9]+)$/)
	    {
		$disk{name}=$1;
		$disk{size}=$2;
		$linecorrect=1;
	    }

	    if ($line =~ /^part=([0-9]+)\ +size=([0-9]+)\ +fdisktype=([0-9a-zA-Z]+)\ +label=([a-zA-Z]+)\ +type=([a-zA-Z]+)$/)
	    {
		($number,$size,$fdisktype,$label,$type)=($1,$2,$3,$4,$5);
		$linecorrect=1;
	    }
		
	    if ($line =~/^part=([0-9]+)\ +size=([0-9]+)\ +label=([a-zA-Z]+)\ +type=([a-zA-Z]+)$/)
	    {
		($number,$size,$label,$type)=($1,$2,$3,$4);
		$linecorrect=1;
	    }
	    if (
		(
		 $type eq "primary"  ||
		 $type eq "logical"
		 ) &&
		$linecorrect
		)
	    {	
		%partition=(
			    number    => $number,
			    size      => $size,
			    fdisktype => $fdisktype,
			    label     => $label,
			    type      => $type,
			    );
		$disk{partition}{$partition{number}}=\%partition;
	    }
	    if ($linecorrect &&
		$type eq "extended")
	    {
		%partition=(
			    number    => $number,
			    size      => $size,
			    label     => $label,
			    type      => $type,
			    );
		$disk{partition}{$partition{number}}=\%partition;
	    }	    
	    $linecorrect=0;
	}
	close(FH);
    }
    else
    {
	print "Error opening $partitionfile\n";
	exit 1;
    }


}

sub printpart()
{
    my $key1;
    my $key2;
    my $key3;
    my $key4;
    my $tmphash1;
    my $tmphash2;
    my $tmphash3;
    foreach $key1 (sort keys %disk)
    {
	$tmphash1 = $disk{$key1};
	if (ref($tmphash1) eq 'HASH')
	{
	    foreach $key2 (sort keys %$tmphash1)
	    {
		print "\t$key1=$key2\n";		    
		$tmphash2 = $$tmphash1{$key2};
		if (ref($tmphash2) eq 'HASH')
		{
		    foreach $key3 (sort keys %$tmphash2) 
		    {
			print "\t\t$key3=".$$tmphash2{$key3}."\n";
		    }
		}
	    }
	}
	else
	{
	    print "$key1=$disk{$key1}\n";
	}
    }
}

sub printfdiskformat()
{
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
    foreach $key1 (sort keys %disk)
    {
	$tmphash1 = $disk{$key1};
	if (ref($tmphash1) eq 'HASH')
	{
	    foreach $key2 (sort keys %$tmphash1)
	    {
#		print "\t$key1=$key2\n";
		$partnumber=$key2;
		$tmphash2 = $$tmphash1{$key2};
		if (ref($tmphash2) eq 'HASH')
		{
		    foreach $key3 (sort keys %$tmphash2) 
		    {
			if ($key3 eq "number")
			{
			    $partnumber=$$tmphash2{$key3};
			}
			if ($key3 eq "fdisktype")
			{
			    $fdisktype=$$tmphash2{$key3}
			}
			if ($key3 eq "size")
			{
			    $size="+$$tmphash2{$key3}M"
			}
			if ($key3 eq "type")
			{
			    if ($$tmphash2{$key3} eq "primary")
			    {
				$type="p";
			    }
			    if ($$tmphash2{$key3} eq "extended")
			    {
				$type="e";
			    }
			    if ($$tmphash2{$key3} eq "logical")
			    {
				$type="l";
			    }


			}
#			print "\t\t$key3=".$$tmphash2{$key3}."\n";			
		    }

		    if ($type eq "p" ||
			$type eq "e")
		    {
			print "n
$type
$partnumber

$size
";
		    }
		    if ($type eq "l")
		    {
			print "n
$type

$size
";			
		    }
		    if (!($type eq "e"))
		    {
			print "t
$partnumber
$fdisktype
";
		    }
		}
	    }
	}
	else
	{
#	    print "$key1=$disk{$key1}\n";
	}
    }
    
}


sub printfdiskdeleteprimary()
{
    print "d
1
d
2
d
3
d
4
";
}

sub printfdiskwrite()
{
    print "p
w
";
}

