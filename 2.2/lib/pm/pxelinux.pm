package libkadeploy2::pxelinux_label;

use strict;
use warnings;


sub new()
{
    my $self;
    $self=
    {
	name   => "",
	kernel => "",
	initrd => "",
	append => "",
    };
    bless $self;
    return $self;
}

sub set_name($)
{
    my $self=shift;
    my $name=shift;
    $self->{name}=$name;
}

sub set_kernel($)
{
    my $self=shift;
    my $kernel=shift;
    $self->{kernel}=$kernel;
}

sub set_initrd($)
{
    my $self=shift;
    my $initrd=shift;
    $self->{initrd}=$initrd;
}

sub set_append($)
{
    my $self=shift;
    my $append=shift;
    $self->{append}=$append;
}

sub get()
{
    my $self=shift;
    my $name=$self->{name};
    my $kernel=$self->{kernel};
    my $initrd=$self->{initrd};
    my $append=$self->{append};

    if (! $name || 
	! $kernel)
    {
	print STDERR "ERROR name or kernel not set !!!!\n";
	exit 1;	
    }

    my $label="
label $name
      kernel $kernel    
";
    if ($initrd && $append)
    {
	$label.="      append initrd=$initrd $append\n\n";
    }
    elsif ($append)
    {
	$label.="      append $append\n\n";
    }
    elsif ($initrd)
    {
	$label.="      append initrd=$initrd\n\n";	
    }

    return $label;
}

1;



package libkadeploy2::pxelinux;

use strict;
use warnings;

sub new()
{
    my $self;
    my %labellist=();
    my $reflabellist=\%labellist;
    
    $self=
    {
	timeout               => 5,
	defaultlabel          => "linux",
	message               => 0,
	label_list            => $reflabellist,
	firstserialportspeed  => 9600,
	secondserialportspeed => 9600,
    };
    bless $self;
    return $self;
}

sub set_serialspeed($$)
{
    my $self=shift;
    my $serialport=shift;
    my $serialportspeed=shift;

    if ($serialport==1)
    {
	$self->{firstserialportspeed}=$serialportspeed;
    }
    if ($serialport==2)
    {
	$self->{secondserialportspeed}=$serialportspeed;
    }
}

sub add($$$$)
{
    my $self=shift;

    my $name=shift;
    my $kernel=shift;
    my $initrd=shift;
    my $append=shift;

    my $label=libkadeploy2::pxelinux_label::new();
    my $ref_label_list=$self->{label_list};
    my %label_list=%$ref_label_list;


    $label->set_name($name);
    $label->set_kernel($kernel);
    $label->set_initrd($initrd);
    $label->set_append($append);
    $label_list{$name}=$label;

    $ref_label_list=\%label_list;
    $self->{label_list}=$ref_label_list;
}

sub set_default($)
{
    my $self=shift;
    
    my $defaultlabel=shift;
    $self->{defaultlabel}=$defaultlabel;
}

sub get()
{
    my $self=shift;

    my $ref_label_list=$self->{label_list};
    my %label_list=%$ref_label_list;
    my $key;
    my $defaultconf;
    my $labelentry;
    my $menu;
    my $defaultlabel;

    my $timeout=$self->{timeout};
    my $firstserialportspeed=$self->{firstserialportspeed};
    my $secondserialportspeed=$self->{secondserialportspeed};
    $defaultconf="serial 0 $firstserialportspeed
serial 1 $secondserialportspeed
prompt 1
timeout $timeout
";

    if ($self->{defaultlabel})
    {
	$defaultlabel=$self->{defaultlabel};
	$defaultconf.="
default $defaultlabel
";

    }



    foreach $key (keys %label_list)
    {
	my $label=$label_list{$key};
	my $str=$label->get();
	$labelentry.=$str;
    }
    
    $menu.=$defaultconf.$labelentry;

    return $menu;
}

1;
