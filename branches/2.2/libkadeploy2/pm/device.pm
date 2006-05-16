package libkadeploy2::device;

my $message=libkadeploy2::message::new();

sub new($$$)
{
    my $type=shift;
    my $disknumber=shift;
    my $partnumber=shift;
    my $self;
    if (! ($type && $disknumber && $partnumber))
    { $message->unknowerror(2,"device.pm new()"); exit 1; }

    $self=
    {
	type       => $type,
	disknumber => $disknumber,
	partnumber => $partnumber,
    };
    bless $self;
    return $self;
}

sub get($)
{
    my $self=shift;
    my $ostype=shift;

    if ($ostype eq "linux")
    { return $self->get_linux(); }
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
	$message->unknowerror(2,"device.pm get_linux()");
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
    
    if ($self->{partnumber})
    { $destdevice.=$self->{partnumber}; }
    return $destdevice;
}

1;
