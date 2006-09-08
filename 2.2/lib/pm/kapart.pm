package libkadeploy2::kapart;

use strict;
use warnings;
use POSIX;
use Getopt::Long;

use libkadeploy2::cmdline;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::command;
use libkadeploy2::nodelist;
use libkadeploy2::remoteparallelcommand;
use libkadeploy2::disk;
use libkadeploy2::device;
use libkadeploy2::disks;
use libkadeploy2::environment;



use libkadeploy2::fdiskscript;
use libkadeploy2::fdiskscriptsolaris;
use libkadeploy2::formatscriptsolaris;
use libkadeploy2::fstab;
use libkadeploy2::kamcat;
    

sub get_options();
sub check_options();
sub kapart($);

my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }

my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();

my $kadeploydir=$conf->get("kadeploy2_directory");
my $kamcatpath=$conf->getpath_cmd("kamcat");
my $kapartpath=$conf->getpath_cmd("kapart");
my $kanodespath=$conf->getpath_cmd("kanodes");

################################################################################

sub new()
{
    my $self;
    $self=
    {
	nodelist        => 0,
	partitionfile   => "",
	
	disknumber      => 0,
	partnumber      => 0,
	
	ostype          => "linux",
	
	dofdisk         => 0,
	printfdisk      => 0,
	printfstab      => 0,
	printformat     => 0,
	printpartitionfile => 0,
	showfdisk       => 0,

	login           => 0,
	envname         => 0,

	verbose         => 0,
    };
    bless $self;
    return $self;
}

sub run()
{
    my $self=shift;
    my $ok=1;
    if (! $self->check_options()) { return 1; }
    if ($self->{dofdisk})
    {
	$ok=$self->dofdisk();
    }
    elsif ($self->{printfdisk})
    {
	$ok=$self->printfdisk();
    }
    elsif ($self->{printfstab})
    {
	$ok=$self->printfstab();
    }
    elsif ($self->{printformat})
    {
	$ok=$self->printformatsolaris();
    }
    elsif ($self->{printpartitionfile})
    {
	$ok=$self->printpartitionfile();
    }
    elsif ($self->{showfdisk})
    {
	$ok=$self->showfdisk();
    }
    else
    {
	$message->message(3,"kapart::run error");
	$ok=0;
    }

    return $ok;
}

sub set_nodelist($)     { my $self=shift; my $arg=shift; $self->{nodelist}=$arg;    }
sub set_disknumber($)   { my $self=shift; my $arg=shift; $self->{disknumber}=$arg;  }
sub set_partnumber($)   { my $self=shift; my $arg=shift; $self->{partnumber}=$arg;  }
sub set_ostype($)       { my $self=shift; my $arg=shift; $self->{ostype}=$arg;  }
sub set_partitionfile($){ my $self=shift; my $arg=shift; $self->{partitionfile}=$arg;  }
sub set_dofdisk()       { my $self=shift; $self->{dofdisk}=1;     }
sub set_login()         { my $self=shift; $self->{login}=shift;     }
sub set_envname()       { my $self=shift; $self->{envname}=shift;     }

sub get_options_cmdline()
{
    my $self=shift;
    my $nodelist;
    my @node_list;
    my $partitionfile;
    my $disknumbercmdline;
    my $partnumbercmdline;
    my $ostype;
    my $dofdisk;
    my $printfdisk=0;
    my $printfstab=0;
    my $printformat=0;
    my $verbose=0;
    my $help;
    my $getopt=0;
    my $debug=0;
    $getopt=GetOptions(
		       'm=s'                  => \@node_list,
		       'machine=s'            => \@node_list,
		       
		       'partitionfile=s'      => \$partitionfile,
		       
		       'disknumber=s'         => \$disknumbercmdline,
		       'd=s'                  => \$disknumbercmdline,
		       
		       'partnumber=s'         => \$partnumbercmdline,	      
		       'p=s'                  => \$partnumbercmdline,	      
		       
		       'ostype=s'             => \$ostype,
		       
		       'fdisk!'               => \$dofdisk,
		       'printfdisk!'          => \$printfdisk,
		       'printfstab!'          => \$printfstab,
		       'printformat!'         => \$printformat,
		       
		       'v!'                   => \$verbose,
		       'verbose!'             => \$verbose,
	       
		       'h!'                   => \$help,
		       'help!'                => \$help,

		       'debug!'               => \$debug,
		       );
    
    if (! $getopt) { $message->message(2,"Checking option"); exit 1; }
    if ($verbose)  { $errorhandler->set_verbose(); }
    if ($debug)    { $message->set_debug(); }

    foreach my $arg (@ARGV) { if ($arg =~ /([a-zA-Z0-9_]+)@([a-zA-Z0-9\._]+)/) 
			      { $self->{login}=$1; $self->{envname}=$2; } }


    if (
	(! $help)
	)
    {	
	if (@node_list)     
	{ $nodelist=libkadeploy2::nodelist::new(); $nodelist->loadlist(\@node_list);  }
	else { $message->missing_cmdline(2,"nodes"); return 0; }
	$self->set_nodelist($nodelist);
    }
    
    if (! $ostype) { $self->set_ostype("linux"); }
    else { $self->set_ostype($ostype); }

    $self->set_disknumber($disknumbercmdline);    
    $self->set_partnumber($partnumbercmdline);
    $self->set_partitionfile($partitionfile);


    if ($dofdisk)      { $self->{dofdisk}=1;     }
    if ($printfdisk)   { $self->{printfdisk}=1;  }
    if ($printfstab)   { $self->{printfstab}=1;  }
    if ($printformat)  { $self->{printformat}=1; }

    $self->{help}=$help;
    $self->{verbose}=$verbose;

    return 1;
}

sub check_options()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $partitionfile=$self->{partitionfile};
    my $flags=0;    
    my $environment;
    my $ok=1;
    if ($self->{help}) { $message->kapart_help(); exit 0;     }

    
    $nodelist=$self->{nodelist};
    $partitionfile=$self->{partitionfile};

    if ($self->{login} && $self->{envname})
    {
	$environment=libkadeploy2::environment::new();
	$environment->set_name($self->{envname});
	$environment->set_user($self->{login});    
	if (! $environment->get_descriptionfile_fromdb()) { $message->message(2,"Environment not registred in db..."); return 0; }
	if (! $environment->load())                       { $message->erroropeningfile(2,$environment->get_descriptionfile()); return 0; }
	if (! $environment->is_set("partitionfile"))      { $message->message(1,"Your configuration file doesn't contain partitionfile"); return 0; }
	$self->{partitionfile}=$environment->get("partitionfile");
	if (! $nodelist) { $self->{printpartitionfile}=1; }
    }


    if (
	(! $nodelist)       && 
	(! $partitionfile)  &&
	(! $self->{printpartitionfile})
	)
    {
	$message->missing_cmdline(2,"partitionfile or nodelist"); 
	return 0;    
    }
	
    if (! $self->{disknumber} && $self->{dofdisk}==1)
    { $self->{disknumber}=1; }


    if ((! $nodelist )&& $self->{dofdisk}==1)
    { $message->missing_cmdline(2,"nodes"); return 0; }


    if (! $self->{ostype})            { $message->missing_cmdline(2,"ostype"); return 0; }
     
    if ($partitionfile &&
	(! -f $partitionfile))
	{ $message->filenotfound(2,$partitionfile); return 0; }


    if (
	$nodelist  &&
	(!$self->{dofdisk})    && 
	(!$self->{printfdisk}) && 
	(!$self->{printfstab}) && 
	(!$self->{printformat})&&
	(!$self->{printpartitionfile})
	)
    {
	$self->{showfdisk}=1; 
    }
    elsif (
	(!$self->{dofdisk})    && 
	(!$self->{printfdisk}) && 
	(!$self->{printfstab}) && 
	(!$self->{printformat})&&
	(!$self->{printpartitionfile})
	)
    {
	$message->message(2,"choose a flags");
	return 0;
    }

    return $ok;
}

sub dofdisk()
{
    my $self=shift;
    if ($self->{ostype} eq "linux")
    { return $self->dofdisk_linux(); }
    elsif ($self->{ostype} eq "solaris")
    { return $self->dofdisk_solaris(); }
    else { return 0; }
}


sub dofdisk_linux()
{
    my $self=shift;
    my $ok=1;
    my $ref_node_list;
    my @node_list;
    my $node;
    my $remoteparallelcommand;
    my $nodename;
    my $i;
    my $device;
    my $linuxdev;
    my $disk;
    my $disks;
    my $partitionfile;
    my $disknumber;
    my $nodelist;
    my $disktype;
    my $cmd;
    my $kamcat=libkadeploy2::kamcat::new();

    $partitionfile=$self->{partitionfile};
    $nodelist=$self->{nodelist};
    $disknumber=$self->{disknumber};

    $disk=libkadeploy2::disk::new();	
    if (! $disk->loadpartitionfile($partitionfile))
    { $message->message(2,"in partition file $partitionfile"); return 0; }

    $disks=libkadeploy2::disks::new($nodelist);
    if (! $disks->check_disk_type($disknumber)) { return 0; }

    $disktype=$disks->get_disk_type($disknumber);


    $kamcat->set_user("root");
    $kamcat->set_srvcmd("$kapartpath --printfdisk --partitionfile $partitionfile ".$nodelist->get_cmdline());
    $kamcat->set_cltcmd("cat > /tmp/fdisk.txt");
    $kamcat->set_nodelist($nodelist);
    $kamcat->run();

    if ($kamcat->run()!=0) { $ok=0; }
    else 
    { 
#	$cmd="$kanodespath --add --partitionfile";
    

	$device=libkadeploy2::device::new();
	$device->set_type($disktype);
	$device->set_disknumber($disknumber);
	$device->set_partnumber(0);
	$linuxdev=$device->get_linux();



	$cmd="\"umount /mnt/dest/*\"";
	$remoteparallelcommand=libkadeploy2::remoteparallelcommand::new();
	$remoteparallelcommand->set_connector("ssh");
	$remoteparallelcommand->set_parallellauncher("internal");
	$remoteparallelcommand->set_login("root");
	$remoteparallelcommand->set_nodelist($nodelist);
	$remoteparallelcommand->set_command($cmd);
	$remoteparallelcommand->set_timeout(30);

	if ($self->{verbose}==1) { $remoteparallelcommand->set_verbose(); }
	$remoteparallelcommand->exec();

	$cmd="\"umount /mnt/dest\"";
	$remoteparallelcommand->set_command($cmd);
	$remoteparallelcommand->exec();



	$cmd="\"cat /tmp/fdisk.txt \| fdisk /dev/$linuxdev\"";
	$remoteparallelcommand->set_command($cmd);
	$ok=$remoteparallelcommand->exec();
    }						    

    
    if ($ok)
    {
	$message->message(0,"Partition done for node ".$nodelist->get_str());
	return $self->registerpartitionfile();
    }
    else
    {
	$message->message(2,"Partition failed for node ".$nodelist->get_str());
	return 0;
    }
}

sub dofdisk_solaris()
{
    my $self=shift;
    my $ok=1;
    my $retcode=0;
    my $ref_node_list;
    my @node_list;
    my $node;
    my $remoteparallelcommand;
    my $nodename;
    my $i;
    my $device;
    my $solarisdev;
    my $disk;
    my $disks;
    my $partitionfile;
    my $disknumber;
    my $nodelist;
    my $disktype;
    my $cmd;
    my $kamcat=libkadeploy2::kamcat::new();

    $partitionfile=$self->{partitionfile};
    $nodelist=$self->{nodelist};
    $disknumber=$self->{disknumber};

    $disk=libkadeploy2::disk::new();	
    if (! $disk->loadpartitionfile($partitionfile))
    { $message->message(2,"in partition file $partitionfile"); return 0; }

    $disks=libkadeploy2::disks::new($nodelist);
    if (! $disks->check_disk_type($disknumber)) { return 0; }

    $disktype=$disks->get_disk_type($disknumber);


    $kamcat->set_user("root");
    $kamcat->set_srvcmd("$kapartpath --printfdisk --ostype solaris --partitionfile $partitionfile ".$nodelist->get_cmdline());
    $kamcat->set_cltcmd("cat > /tmp/root/etc/fdisk.txt");
    $kamcat->set_nodelist($nodelist);
    $retcode=$kamcat->run();

    if ($retcode==0)
    {
	$kamcat->set_user("root");
	$kamcat->set_srvcmd("$kapartpath --printformat --ostype solaris --partitionfile $partitionfile ".$nodelist->get_cmdline());
	$kamcat->set_cltcmd("cat > /tmp/root/etc/format.txt");
	$kamcat->set_nodelist($nodelist);
	$retcode=$kamcat->run();
    }
    else { $ok=0; }


    if ($ok==0) { $message->message(2,"kapart error"); }
    else 
    { 
#	$cmd="$kanodespath --add --partitionfile";
    


	$device=libkadeploy2::device::new();
	$device->set_type($disktype);
	$device->set_disknumber($disknumber);
	$device->set_partnumber(0);
	$device->set_char();
	$device->set_ostype("solaris");

	$solarisdev=$device->get();


	$cmd="\"umount /mnt/dest/*\"";
	$remoteparallelcommand=libkadeploy2::remoteparallelcommand::new();
	$remoteparallelcommand->set_connector("ssh");
	$remoteparallelcommand->set_parallellauncher("internal");
	$remoteparallelcommand->set_login("root");
	$remoteparallelcommand->set_nodelist($nodelist);
	$remoteparallelcommand->set_command($cmd);
	$remoteparallelcommand->set_timeout(30);

	if ($self->{verbose}==1) { $remoteparallelcommand->set_verbose(); }
	$remoteparallelcommand->exec();

	$cmd="\"umount /mnt/dest\"";
	$remoteparallelcommand->set_command($cmd);
	$remoteparallelcommand->exec();



	$cmd="\"cat /tmp/root/etc/fdisk.txt \| fdisk /dev/$solarisdev\"";
	$remoteparallelcommand->set_command($cmd);
	$ok=$remoteparallelcommand->exec();

	if ($ok)
	{
	    $cmd="\"cat /tmp/root/etc/format.txt \| format\"";
	    $remoteparallelcommand->set_command($cmd);
	    $ok=$remoteparallelcommand->exec();
	}
    }						    
    
    if ($ok)
    {
	$message->message(0,"Partition done for node ".$nodelist->get_str());
	return $self->registerpartitionfile();
    }
    else
    {
	$message->message(2,"Partition failed for node ".$nodelist->get_str());
	return 0;
    }

}

sub registerpartitionfile()
{
    my $self=shift;
    my $partitionfile=$self->{partitionfile};
    my $nodelist=$self->{nodelist};
    my $node;
    my $nodename;
    my $disk;	
    my $disknumber=1;
    my $ok=1;
    my $ref_node_list;
    my @node_list;

    $disk=libkadeploy2::disk::new();


    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;

    foreach $node (@node_list)
    {
	$nodename=$node->get_name();
	if ($disk->loadpartitionfile($partitionfile))
	{
	    $disk->loaddisksetting($nodename,1);
	    $disk->addtodb($nodename,1);
	}
	else
	{
	    $ok=0;
	}
    }
    return $ok;
}

sub printfdisk()
{
    my $self=shift;
    my $partitionfile=$self->{partitionfile};
    my $ostype=$self->{ostype};

    if ($ostype eq "linux")
    {
	return $self->printfdisklinux();
    }
    elsif ($ostype eq "solaris")
    {
	return $self->printfdisksolaris();
    }
    else
    {
	return 0;
    }
}

sub printfdisklinux()
{
    my $self=shift;
    my $partitionfile=$self->{partitionfile};
    my $disk;	
    my $fdiskscript;
    my $ok=1;
    $disk=libkadeploy2::disk::new();
    $fdiskscript=libkadeploy2::fdiskscript::new();
    if ($disk->loadpartitionfile($partitionfile))
    {
	$fdiskscript->set_disk($disk);
	$ok=$fdiskscript->print();
    }
    else
    {
	$ok=0;
    }
    return $ok;
}

sub printfdisksolaris()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $node;
    my $nodename;
    my $disk;	
    my $fdiskscript;
    my $disknumber=1;
    my $ok=1;
    $disk=libkadeploy2::disk::new();
    $fdiskscript=libkadeploy2::fdiskscriptsolaris::new();
    if (! $nodelist)
    {
	$message->missing_cmdline(1,"nodes");
	$ok=0;
    }
    else
    {
	$node=$nodelist->get_node(0);
	$nodename=$node->get_name();
	if ($disk->get_fromdb($nodename,$disknumber))
	{
	    $disk->loadpartitionfile($self->{partitionfile});
	    $fdiskscript->set_disk($disk);
	    $ok=$fdiskscript->print();
	}
	else
	{
	    $ok=0;
	}
	return $ok;
    }
}

sub printformatsolaris()
{
    my $self=shift;
    my $partitionfile=$self->{partitionfile};
    my $disk;	
    my $formatscript;
    my $ok=1;
    $disk=libkadeploy2::disk::new();
    $formatscript=libkadeploy2::formatscriptsolaris::new();
    if ($disk->loadpartitionfile($partitionfile))
    {
	$formatscript->set_disk($disk);
	$ok=$formatscript->print();
    }
    else
    {
	$ok=0;
    }
    return $ok;
}



sub printfstab()
{
    my $self=shift;
    my $partitionfile=$self->{partitionfile};
    my $nodelist=$self->{nodelist};
    my $disknumber=$self->{disknumber};
    my $partnumber=$self->{partnumber};
    my $ostype=$self->{ostype};
    my $diskinterface;
    my $disk;
    my $disks;
    my $fstab;
    my $fstabbuffer="";
    my $ok=1;
    my $disktype;    

    $disks=libkadeploy2::disks::new($nodelist);
    if (! $disks->check_disk_type($disknumber)) { return 0; }
    $disktype=$disks->get_disk_type($disknumber);


    $disk=libkadeploy2::disk::new();
    if (!$disk->loadpartitionfile($partitionfile))
    {
	$message->message(2,"in partitionfile");
	return $ok;
    }
    $disk->set_interface($disktype); 
    $fstab=libkadeploy2::fstab::new();
    $fstab->add_disk(1,$disk);
    $fstab->set_bootdisknumber($disknumber);
    $fstab->set_bootpartnumber($partnumber);
    $fstab->set_ostype($ostype);
    $fstabbuffer=$fstab->get();
    if ($fstabbuffer)
    { 	print $fstabbuffer;  }
    else { $ok=0; }
    return $ok;
}

sub printpartitionfile()
{
    my $self=shift;
    my $ok=1;
    if ($self->{partitionfile} &&
	-f $self->{partitionfile})
    {
	$message->message(-1,"showing partition file for ".$self->{login}."@".$self->{envname});
	$message->message(-1,"filepath: ".$self->{partitionfile});
	open FH, $self->{partitionfile} or $ok=0;
	while (my $line=<FH>) { print $line; }
    }
    else 
    { $ok=0; }
    return $ok;
}


sub showfdisk()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $disknumber=$self->{disknumber};
    my $disk;	
    my $fdiskscript;
    my $ok=1;
    my $node;
    my $refnodelist=$nodelist->get_nodes();
    my @node_list=@$refnodelist;

    foreach $node (@node_list)
    {
	$disk=libkadeploy2::disk::new();
	if ($disk->get_fromdb($node->get_name(),1))
	{
	    $disk->print();
	}
	else
	{
	    $ok=0;
	}
    }
    return $ok;
}


1;
