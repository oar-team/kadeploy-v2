package libkadeploy2::device;

my $message=libkadeploy2::message::new();

sub new()
{
    my $self;

    $self=
    {
	type       => 0,
	disknumber => -1,
	partnumber => -1,
	slicenumber=> -1,
	ostype     => "linux",
	char       => 0,
	block      => 1,
    };
    bless $self;
    return $self;
}


sub set_char()        { my $self=shift; $self->{char}=1; $self->{block}=0; }
sub set_block()       { my $self=shift; $self->{block}=1; $self->{char}=0; }
sub set_disknumber($) { my $self=shift; $self->{disknumber}=shift;  }
sub set_partnumber($) { my $self=shift; $self->{partnumber}=shift;  }
sub set_slicenumber($){ my $self=shift; $self->{slicenumber}=shift;  }
sub set_ostype($)     { my $self=shift; $self->{ostype}=shift;      }
sub set_type($)       { my $self=shift; $self->{type}=shift;        }

sub check()
{
    my $self=shift;
    my $ok=1;
    if (! ($self->{type})) 
    { $message->message(2,"Unknow disk interface device.pm get_linux()"); $ok=0; }

    if (! ($self->{type} && 
	   $self->{disknumber}))
    { 
	$message->message(2,"device.pm check() type:$type disknumber:$disknumber partnumber:$partnumber"); 
	$ok=0; 
    }
    return $ok;
}

sub get()
{
    my $self=shift;
    my $ostype=$self->{ostype};

    if (! $self->check()) { $message->message(3,"device::check failed"); }

    if ($ostype eq "linux")
    { return $self->get_linux(); }
    elsif ($ostype eq "solaris")
    { return $self->get_solaris(); }
    else
    {
	$message->osnotsupported($ostype);
	exit 1;
    }
}

sub get_linux()
{
    my $self=shift;
    my $destdevice;

    if ($self->{type} eq "ide")
    { 	$destdevice.="hd";     }
    elsif ($self->{type} eq "scsi" ||
	   $self->{type} eq "sata")
    { 	$destdevice.="sd";     }
    else  
    {
	$message->unknowerror(2,"type:".$self->{type}." device.pm get_linux()");
	exit 1;
    }

    if ($self->{disknumber} == 1)
    { $destdevice.="a"; }
    elsif ($self->{disknumber} == 2)
    { $destdevice.="b"; }
    elsif ($self->{disknumber} == 3)
    { $destdevice.="c"; }
    elsif ($self->{disknumber} == 4)
    { $destdevice.="d"; }
    else  
    {
	$message->unknowerror(2,"device.pm get_linux()");
	exit 1;
    }
    
    if ($self->{partnumber} > 0)
    { $destdevice.=$self->{partnumber}; }
    return $destdevice;
}

sub get_solaris()
{
    my $self=shift;
    my $destdevice="";
    my $disknumber=$self->{disknumber}-1;
    my $slicenumber=$self->{slicenumber}-1;

    if ($self->{char})
    { $destdevice="rdsk/c0d"; }
    elsif ($self->{block})
    { $destdevice="dsk/c0d";  }
    else
    { $message->unknowerror(3,"device.pm get_solaris()"); exit 1; }

    if (! ($self->{type})) { $message->message(2,"Unknow disk interface device.pm get_linux()"); return 0; }

    if ($self->{disknumber}>0 && 
	$self->{disknumber}<5
	)
    {
	$destdevice.=$disknumber;
    }
    else  
    {
	$message->unknowerror(3,"device.pm get_solaris()");
	exit 1;
    }
    
    if ($self->{slicenumber} >= 0)
    { $destdevice.="s".$self->{slicenumber}; }
    elsif ($self->{partnumber} >=0)
    { $destdevice.="p".$self->{partnumber}; }
    return $destdevice;
}


1;
