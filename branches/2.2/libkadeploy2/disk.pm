package libkadeploy2::disk;
use strict;
use warnings;
use libkadeploy2::deploy_iolib;

sub get_fromdb($$);

sub new()
{
    my $self;
    $self=
    {
    };
    bless $self;
    return $self;
}

sub loadfile($)
{
    my $self=shift;
    my $partitionfile=shift;
    my $line;
    my %disk;
    my %partitions;
    $self->{partition}=\%partitions; 

    my ($number,$size,$fdisktype,$label,$type);
    my $linecorrect=0;
    my @listnumber;
    my $sizeofextended=0;
    my $sizeoflogical=0;
    my $sizeofprimary=0;
    my $numberofprimary=0;
    my $numberofextended=0;
    my $numberoflogical=0;

    my $tmpnumber;
    $type="";

    if (open(FH,$partitionfile))
    {
	foreach $line (<FH>)
	{
	    my $linenotseen=1;
	    my %partition;

	    if ($numberofextended > 1) { print STDERR "ERROR you can only have one extended part\n"; exit 1; }
	    if ($numberofprimary > 4   ) { print STDERR "ERROR you can only have four primary part\n"; exit 1; }
	    if ($numberofprimary + $numberofextended > 4   ) { print STDERR "ERROR you can only have four part (primary + extended)\n"; exit 1; }
	    if ($numberofextended == 0 && $numberoflogical > 0 ) 
	    { print STDERR "ERROR extended partition must be defined before logical\n"; exit 1; }

	    if($line =~ /^(ide|scsi|sata)[\s\t]+size=([0-9]+)[\t\s]*$/)
	    {
		$type=$1;
		$size=$2;
		$self->set_interface($type);
		$self->set_size($size);
		$linecorrect=1;
	    }

	    if ($line =~ /^part=([0-9]+)[\t\s]+size=([0-9]+)[\t\s]+fdisktype=([0-9a-zA-Z]+)[\t\s]+label=([a-zA-Z]+)[\t\s]+type=([a-zA-Z]+)[\t\s]*$/)
	    {
		($number,$size,$fdisktype,$label,$type)=($1,$2,$3,$4,$5);
		if ($type eq "logical" && $number < 4) { print STDERR "ERROR logical part begin at 5\n"; $linecorrect=0;}
		if ($type eq "primary" && $number > 4) { print STDERR "ERROR primary part must be [1..4]\n"; $linecorrect=0;}
		if ($type eq "primary") { $numberofprimary++; $sizeofprimary+=$size; }
		if ($type eq "logical") { $sizeoflogical+=$size; $numberoflogical++; }
		foreach $tmpnumber (@listnumber)
		{
		    if ($tmpnumber==$number) { print STDERR "ERROR part=$number alreayd defined\n"; $linecorrect=0; }
		}
		@listnumber=(@listnumber,$number);
		$self->setpartition($number,$size,$fdisktype,$label,$type);
		$linecorrect=1;
	    }
		
	    if ($line =~/^part=([0-9]+)[\t\s]+size=([0-9]+)[\t\s]+type=(extended)[\t\s]*$/)
	    {
		($number,$size,$type)=($1,$2,$3,$4);
		if ($number > 4) { print STDERR "ERROR extended partition must be in [1..4]\n"; $linecorrect=0; }
		if ($type eq "extended") { $numberofextended++; $sizeofextended=$size; }
		foreach $tmpnumber (@listnumber)
		{
		    if ($tmpnumber==$number) { print STDERR "ERROR part=$number already defined\n"; $linecorrect=0; }
		}
		@listnumber=(@listnumber,$number);
		$self->setpartition($number,$size,$fdisktype,$label,$type);
		$linecorrect=1;
	    }
	    if ($line =~ /^$/ ||
		$line =~ /^\#/)
	    { $linecorrect=1; }

	    if ($linecorrect==0)
	    {
		print STDERR "ERROR in line :\n$line";
		exit 1;
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
    if ($sizeoflogical >= $sizeofextended &&
	$numberofextended > 0 &&
	$numberoflogical  > 0	
	) 
    { print STDERR "ERROR size of extended part <= size of logical part\nsizeofextended $sizeofextended\nsizeoflogical $sizeoflogical\n"; exit 1; }   
}

sub sort_par_num { return $a <=> $b }

sub print()
{
    my $self=shift;
    my $key1;
    my $key2;
    my $key3;
    my $key4;
    my $tmphash1;
    my $tmphash2;
    my $tmphash3;
    foreach $key1 (sort keys %$self)
    {
	$tmphash1 = $self->{$key1};
	if (ref($tmphash1) eq 'HASH')
	{
	    foreach $key2 (sort sort_par_num keys %$tmphash1)
	    {
		print "\t\t$key1=$key2\n";		    
		$tmphash2 = $$tmphash1{$key2};
		if (ref($tmphash2) eq 'HASH')
		{
		    foreach $key3 (sort keys %$tmphash2) 
		    {
			print "\t\t\t$key3=".$$tmphash2{$key3}."\n";
		    }
		}
	    }
	}
	else
	{
	    print "\t$key1=$self->{$key1}\n";
	}
    }
}

sub set_interface($)
{
    my $self=shift;
    my $interface=shift;
    $self->{interface}=$interface;
}

sub set_size($)
{
    my $self=shift;
    my $size=shift;
    $self->{size}=$size;
}

sub get_interface()
{
    my $self=shift;
    return $self->{interface};
}

sub get_size()
{
    my $self=shift;
    return $self->{size};
}


sub get_fromdb($$)
{
    my $self=shift;
    my $nodename=shift;
    my $disknumber=shift;
    my $nodeid;
    my $db;
    my $diskid;
    my %info;
    my $refinfo;
    my $reflistpartitionid;
    my @listpartitionid;
    my $partitionid;

    $db = libkadeploy2::deploy_iolib::new();
    $db->connect();
    $nodeid=$db->node_name_to_id($nodename);
    if ($nodeid)
    {
	$diskid=$db->get_diskid_from_nodeid_disknumber($nodeid,$disknumber);
	if ($diskid)
	{
	    $refinfo=$db->get_diskinfo_from_diskid($diskid);
	    %info=%$refinfo;
	    $self->set_interface($info{interface});
	    $self->set_size($info{size});
			   

	    $reflistpartitionid=$db->get_listpartitionid_from_diskid($diskid);
	    @listpartitionid=@$reflistpartitionid;
	    foreach $partitionid    (@listpartitionid)
	    {
		$refinfo=$db->get_partitioninfo_from_partitionid($partitionid);
		%info=%$refinfo;
		$self->setpartition($info{pnumber},
				    $info{size},
				    0,
				    "",				
				    $info{parttype});
	    }
	}   
	else
	{
	    print STDERR "Disk not found in DB...\n";
	}
    }
    else
    {
	print STDERR "Node not found in DB...\n";
    }
}

sub setpartition($$$$$)
{
    my $self=shift;
    my $number=shift;
    my $size=shift;
    my $fdisktype=shift;
    my $label=shift;
    my $type=shift;
    my %partition;
    my $ok=0;
    
    if ($type eq "primary"  ||
	$type eq "logical"
	)
	{	
	    %partition=(
			number    => $number,
			size      => $size,
			fdisktype => $fdisktype,
			label     => $label,
			type      => $type,
			);
	    $self->{partition}{$partition{number}}=\%partition;
	    $ok=1;
	}

	if ($type eq "extended")
	{
	    %partition=(
			number    => $number,
			size      => $size,
			label     => $label,
			type      => $type,
			);
	    $self->{partition}{$partition{number}}=\%partition;
	    $ok=1;
	}	  
#    if ($ok)
#    {
#	$self->{partition}{$number};
#    }
    return $ok;
}

sub addtodb($$)
{
    my $self=shift;
    my $nodename=shift;
    my $disknumber=shift;
    $self->adddisktodb($nodename,$disknumber);
    $self->addpartitiontodb($nodename,$disknumber);
}

sub adddisktodb($$)
{
    my $self=shift;
    my $nodename = shift;
    my $disknumber = shift;
    my $tmphash1;
    my $size;
    my $interface;
    my $key1;
    my $db;
    my $nodeid;
    my @info;
    my $ok;
    my $disk_id;

    $ok=0;

    foreach $key1 (sort keys %$self)
    {
	if ($key1 eq "size") { $size=$self->{size}; }
	if ($key1 eq "interface") { $interface=$self->{interface}; }
    }

    if ($size && $interface)
    {
	$db=libkadeploy2::deploy_iolib::new();
	$db->connect();
	$nodeid=$db->node_name_to_id($nodename);
	@info = ($disknumber,$interface,$size,$nodeid);
	$disk_id = $db->add_disk(\@info);
	if ($disk_id) { $ok=1; }
	$db->disconnect();    
    }

    if ($ok)
    {
	print "Register $interface harddisk $disknumber for node $nodename\n";
    }
    else
    {
	print "Fail to register $interface harddisk $disknumber for node $nodename\n";
    }
    return $ok;
}


sub addpartitiontodb($$)
{
    my $self=shift;
    my $nodename = shift;
    my $disknumber = shift;
    my $key1;
    my $key2;
    my $key3;
    my $key4;
    my $tmphash1;
    my $tmphash2;
    my $tmphash3;
    my $nodeid;
    my $db;
    my $diskid;
    my @info;
    my $size;
    my $part_id;
    
    my $fdisktype;
    my $label;
    my $number;
    my $type;
    my $ok;    

    $ok=0;

    foreach $key1 (sort keys %$self)
    {
	$tmphash1 = $self->{$key1};
	if (ref($tmphash1) eq 'HASH')
	{
	    foreach $key2 (sort sort_par_num keys %$tmphash1)
	    {
		$tmphash2 = $$tmphash1{$key2};
		if (ref($tmphash2) eq 'HASH')
		{

		    foreach $key3 (sort keys %$tmphash2) 
		    {
			if ($key3 eq "fdisktype") { $fdisktype=$$tmphash2{$key3}; }
			if ($key3 eq "label")     { $label=$$tmphash2{$key3}; }
			if ($key3 eq "number")    { $number=$$tmphash2{$key3}; }
			if ($key3 eq "type")      { $type=$$tmphash2{$key3}; }
			if ($key3 eq "size")      { $size=$$tmphash2{$key3}; }
			if ($fdisktype && 
			    $label     && 
			    $number    &&
			    $type      &&
			    $size
			    )
			{
			    $db = libkadeploy2::deploy_iolib::new();
			    $db->connect();
			    $nodeid=$db->node_name_to_id($nodename);
			    $diskid=$db->nodename_disknumber_to_diskid($nodename,$disknumber);
			    @info=($number,$size,$type,$diskid);
			    $part_id = $db->add_partition(\@info);
			    if ($part_id) { print "Registring partition $number for disk $disknumber of node $nodename\n"; }
			    else          { print "Fail to register partition $number for disk $disknumber of node $nodename\n"; }

			    $fdisktype=undef;
			    $label=undef;
			    $number=undef;
			    $type=undef;
			    $db->disconnect();
			    if ($part_id) {$ok=1; } else { return $ok; }
			}
		    }
		}
	    }
	}
    }
    return $ok;
}

sub getsizeofpartition($)
{
    my $self=shift;
    my $partnumber=shift;
    my $size;
    my %partition;
    my $refpartition;
    $refpartition=$self->{partition}{$partnumber};
    %partition=%$refpartition;
    return $partition{size};
}

sub gettypeofpartition($)
{
    my $self=shift;
    my $partnumber=shift;
    my $size;
    my %partition;
    my $refpartition;
    $refpartition=$self->{partition}{$partnumber};
    %partition=%$refpartition;
    return $partition{type};
}


1;
