package libkadeploy2::kadeployenv;
use strict;
use warnings;
use Getopt::Long;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::environment;
use libkadeploy2::deploy_iolib;
use libkadeploy2::deployconf;
use libkadeploy2::nodelist;
use libkadeploy2::command;
use libkadeploy2::sudo;
use libkadeploy2::karights;
use libkadeploy2::kachecknodes;
use libkadeploy2::time;
use libkadeploy2::kapart;

use libkadeploy2::deploymethod::dd;
use libkadeploy2::deploymethod::linux;
use libkadeploy2::deploymethod::windows;
use libkadeploy2::deploymethod::solaris;
use libkadeploy2::deploymethod::nfsrootlinux;

sub get_options_cmdline();
sub check_options();
sub partitionlinuxnodes();
sub deploylinux();
sub deploypxelinux();
sub deploydd();
sub deploywindows();
sub setuppxelinux();
sub setuppxewindows();
sub setupgrubchainload();
sub check_necessary_right_or_exit($);
sub check_disk_partnumber();

my $errorhandler=libkadeploy2::errorhandler::new();
my $message=libkadeploy2::message::new();
my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }
my $kadeploydir=$conf->get("kadeploy2_directory");
my $db=libkadeploy2::deploy_iolib::new();

my $ddpath=$conf->getpath_cmd("environment_dd");
my $linuxpath=$conf->getpath_cmd("environment_linux");
my $windowspath=$conf->getpath_cmd("environment_windows");

my $kapxe=$conf->getpath_cmd("kapxe");
#my $kapart=$conf->getpath_cmd("kapart");

my $defaultdisknumber=$conf->get("default_disk_number");
my $defaultpartnumber=$conf->get("default_partition_number");

my $defaultpartitioning="/etc/kadeploy/clusterpartition.conf";

sub run()
{
    my $self=shift;
    my $ok=1;
    my $retcode=0;
    my $timedeploy;
    my $partitionfile;

    if (! $self->check_options()) { return 1; }

    my $disknumber=$self->{disknumber};
    my $partnumber=$self->{partnumber};
    my $envname=$self->{envname};
    my $login=$self->{login};
    my $environment=$self->{environment};

    
    $timedeploy=libkadeploy2::time::new();
    $timedeploy->start();

    
#    $envfile=$environment->get_descriptionfile();

    if (!$self->check_necessary_right("PXE")) { return 1; }
    if (!$self->check_envfile())              { return 1; }

    
    #Check nodes if deploytype != PXE
    if (! ($environment->get("deploytype") =~/^pxe.+|nfsroot.+/))
    {

	if (( ! $self->{partnumbercmdline}) && (! $environment->is_set("partnumber")))
	{
	    $message->message(2,"no partnumber defined");
	    return 1;
	}
	if (! $environment->is_set("pxetype"))
	{
	    $message->message(2,"pxetype not set in your envfile... can't setup pxe");
	    return 1;
	}

	my $partitionrightcheck=1;
	if ($environment->is_set("partitionfile"))
	{ if (!
	      (
	       $self->check_necessary_right("DISK$disknumber") &&
	       $self->check_necessary_right("DISK".$disknumber."_PART".$partnumber) &&
	       $self->check_disk_partnumber()
	       )
	      )
	    { $partitionrightcheck=0; }
	}
	else
	{ if (!
	      (
	       $self->check_necessary_right("DISK".$disknumber."_PART".$partnumber) &&
	       $self->check_disk_partnumber()
	       )
	      )
	  { $partitionrightcheck=0; }	  
	}
	if (! $partitionrightcheck) 
	{ $message->message(2,"Missing right on disk $disknumber part $partnumber"); return 1; }


	my $missingnode=1;
	for (my $i=0; $i<5 && $missingnode;$i++)
	{	    
	    $missingnode=0;
	    if (!$self->checknodes_icmp_ssh_mcat())
	    { $message->message(1,"All node are not there"); $missingnode=1; }
	    if (!$self->checknodes_db())
	    { $message->message(-1,"Removing missing nodes"); $missingnode=1; }
	}
	if ($self->{nodelist}->get_numberofnode() == -1)
	{
	    $message->message(2,"There isn't any node...");
	    return 1;
	}

	#PARTITIONING
	$partitionfile=$environment->get("partitionfile");
	if (! $partitionfile)    
	{
	    $message->message(0,"Trying with default parititioning schema for node ".$self->{nodelist}->get_str());
	    $partitionfile=$defaultpartitioning;
	}
	$self->{partitionfile}=$partitionfile;

	$message->message(0,"Partitioning nodes ".$self->{nodelist}->get_str()." whith file $partitionfile");

	if ($environment->is_set("deploytype") &&
	    $environment->is_set("partitionfile")
	    )
	{ $ok=$self->partitionnodes(); 	}
	

	if (! $ok) 
	{ 
	    $message->message(2,"fail to partition some nodes....");
	    return 1; 
	}
	$ok=$self->deploy($environment->get("deploytype"));
	if ($ok) 
	{ 
	    $message->message(0,"Setup pxe for ".$self->{nodelist}->get_str());
	    if (!$self->confsetuppxe($environment->get("deploytype"))) { $message->message(2,"Something wrong in your environment file"); $ok=0; }
	    $ok=$self->setuppxe();
	}	
    }  
    elsif ($environment->get("deploytype") =~ /^pxe.+/)
    { 	    
	if (!$self->confsetuppxe($environment->get("deploytype"))) 
	{ $message->message(2,"Something wrong in your environment file"); $ok=0; }
	if ($ok) { $ok=$self->setuppxe(); }
    }
    elsif ($environment->get("deploytype") =~ /^nfsroot.+/)
    { 	    
	if (!$self->confsetuppxe($environment->get("deploytype"))) 
	{ $message->message(2,"Something wrong in your environment file"); $ok=0; }
	$ok=$self->deploy($environment->get("deploytype"));
	if ($ok) { $ok=$self->setuppxe(); }
    }
       
    if ($ok)
    { 
	$message->message(0, "Deployment success with ".$self->{nodelist}->get_str()); 
	$message->message(-1,"Deployment time ".int($timedeploy->stop())."s");
	$retcode=0; 
    }
    else
    { 
	$message->message(2, "Deployment failed with ".$self->{nodelist}->get_str());  
	$message->message(-1,"Deployment time ".int($timedeploy->stop())."s");
	$retcode=1;
    }

    return $retcode;
}

################################################################################

sub set_login($)        { my $self=shift; my $arg=shift; $self->{envlogin}=$arg;       }
sub set_envname($)      { my $self=shift; my $arg=shift; $self->{envname}=$arg;     }

sub set_disknumber($)   { my $self=shift; my $arg=shift; $self->{disknumbercmdline}=$arg;  }
sub set_partnumber($)   { my $self=shift; my $arg=shift; $self->{partnumbercmdline}=$arg;  }

sub set_nodelist($)    { my $self=shift; my $arg=shift; $self->{nodelist}=$arg;    }

sub set_deploytype($)   { my $self=shift; my $deploytype=shift; $self->{deploytype}=$deploytype; }
sub set_pxetype($)      { my $self=shift; my $pxetype=shift;    $self->{pxetype}=$pxetype; } 
sub set_bootfromnetwork { my $self=shift; $self->{bootfromnetwork}=1; $self->{bootfromdisk}=0; }
sub set_bootfromdisk    { my $self=shift; $self->{bootfromdisk}=1; $self->{bootfromnetwork}=0; }


sub new()
{
    my $self;
    $self=
    {
	login           => "",
	envname         => "",
	nodelist        => "",
	environment     => "",
	help            => 0,
	verbose         => 0,
	bootfromnetwork => 0,
	bootfromdisk    => 0,
	connector       => "ssh",	

	disknumbercmdline => 0,
	partnumbercmdline => 0,
	slicenumbercmdline=> 0,

	disknumber        => 0,
	partnumber        => 0,
    };
    bless $self;
    return $self;
}



sub get_options_cmdline()
{
    my $self=shift;
    
    my @node_list=();
    my $nodelist;
    my $envlogin="";
    my $help;
    my $verbose=0;
    my $envname="";
    my $disknumbercmdline=0;
    my $partnumbercmdline=0;
    my $slicenumbercmdline=0;
    my $getopt=0;
    my $debug=0;

    $getopt=GetOptions(
		       'm=s'                  => \@node_list,
		       'machine=s'            => \@node_list,
		       
		       'disknumber=s'         => \$disknumbercmdline,
		       'd=s'                  => \$disknumbercmdline,
		       'partnumber=s'         => \$partnumbercmdline,
		       'p=s'                  => \$partnumbercmdline,
		       'slicenumber=s'        => \$slicenumbercmdline,
		       's=s'                  => \$slicenumbercmdline,
		       
		       'login=s'              => \$envlogin,
		       'l=s'                  => \$envlogin,
		       
		       'environment=s'        => \$envname,
		       'e=s'                  => \$envname,
		       
		       'h!'                   => \$help,
		       'help!'                => \$help,
		       'v!'                   => \$verbose,
		       'verbose!'             => \$verbose,

		       'debug!'               => \$debug,
		       );
    
    if (! $getopt) { $errorhandler->wrong_parameters_commandline(); }

    if (! $envlogin && 
	! $envname)
    {
	foreach my $arg (@ARGV) { if ($arg =~ /([a-zA-Z0-9_]+)@([a-zA-Z0-9\._]+)/) 
				  { $envlogin=$1; $envname=$2; } }
    }



    if ($verbose)  { $errorhandler->set_verbose(); $self->{verbose}=1; }
    if ($debug)    { $message->set_debug(); }

    if (! $help) 
    {	
	if (@node_list)     
	{ $nodelist=libkadeploy2::nodelist::new(); $nodelist->loadlist(\@node_list);  }	
	$self->{nodelist}=$nodelist;
    }


    if ($partnumbercmdline &&
	$partnumbercmdline=~/[hs]d[a-d](\d)*/) { $partnumbercmdline=$1; }

    if (! $disknumbercmdline) { $disknumbercmdline=1; }

    $self->{disknumbercmdline}=$disknumbercmdline;
    $self->{partnumbercmdline}=$partnumbercmdline;
    $self->{slicenumbercmdline}=$slicenumbercmdline;
    $self->{envlogin}=$envlogin;
    $self->{envname}=$envname;
    $self->{help}=$help;    
}

sub check_options()
{
    my $self=shift;
    my $login=$self->{envlogin};
    my $envname=$self->{envname};
    my $environment;
    my $ok=1;
    my $bootfrom=0;

    if ($self->{help})       { $message->kadeployenv_help(); exit 0; }
    if (! $self->{nodelist}) { $message->missing_cmdline("nodes"); exit 1; }


    if (! $login)                   
    { $message->missing_cmdline(2,"User name needed"); return 0; }

    if (! $envname)                 
    { $message->missing_cmdline(2,"Environment name needed"); return 0; }    

    $environment=libkadeploy2::environment::new();
    $environment->set_name($envname);
    $environment->set_user($login);    
    if (! $environment->get_descriptionfile_fromdb()) { $message->message(2,"Environment not registred in db..."); return 0; }
    if (! $environment->load())                       { $message->erroropeningfile(2,$environment->get_descriptionfile()); return 0; }
    if (! $environment->is_set("deploytype"))         { $message->message(2,"Your configuration file doesn't contain deploytype"); return 0; }
    $self->{envfile}=$environment->get_descriptionfile();

    if ($environment->is_set("partnumber")) 
    { $self->{partnumber}=$environment->get("partnumber"); }
    elsif ($self->{partnumbercmdline})
    { $self->{partnumber}=$self->{partnumbercmdline}; }
    else
    { $self->{partnumber}=$defaultpartnumber; }

    if ($environment->is_set("disknumber"))
    { $self->{disknumber}=$environment->get("disknumber"); }
    elsif ($self->{disknumbercmdline})
    { $self->{disknumber}=$self->{disknumbercmdline}; }
    else
    { $self->{disknumber}=$defaultdisknumber; }	

    if ($environment->get("deploytype") eq "nfsrootlinux")
    { 	$self->set_bootfromnetwork();      }

    
    if  ($environment->is_set("bootfromdisk") &&
	$environment->get("bootfromdisk") eq "yes")
    { $self->set_bootfromdisk(); $bootfrom=1;}
    elsif ($environment->is_set("bootfromnetwork") &&
	$environment->get("bootfromnetwork") eq "yes")
    { $self->set_bootfromnetwork(); $bootfrom=1;}
    
    if (! $bootfrom) 
    { 
	$message->message(2,"bootfromnetwork or bootfromdisk not set in your envfile");
	$ok=0;
    }

    $self->{environment}=$environment;

    return $ok;
}



sub checknodes_icmp_ssh_mcat()
{
    my $self=shift;
    my $kachecknodes;
    my @icmptype=("ICMP");
    my @sshtype=("SSH");
    my @mcattype=("MCAT");
    
    $kachecknodes=libkadeploy2::kachecknodes::new();
    $kachecknodes->set_check();
    $kachecknodes->set_type_list(\@icmptype);
    $kachecknodes->set_retry(10);
    $kachecknodes->set_sleeptime(10);
    $kachecknodes->set_nodelist($self->{nodelist});
    if ($kachecknodes->run()!=0) 
    { 
	$message->message(2,"check @icmptype, node not found... (".$self->{nodelist}->get_str().")"); 
	return 0; 
    }
    
    $kachecknodes=libkadeploy2::kachecknodes::new();
    $kachecknodes->set_check();
    $kachecknodes->set_type_list(\@sshtype);
    $kachecknodes->set_retry(3);
    $kachecknodes->set_sleeptime(10);
    $kachecknodes->set_nodelist($self->{nodelist});
    if ($kachecknodes->run()!=0) 
    { 
	$message->message(2,"check @sshtype, node not found... (".$self->{nodelist}->get_str().")"); 
	return 0; 
    }
    
    $kachecknodes->set_check();
    $kachecknodes->set_type_list(\@mcattype);
    $kachecknodes->set_retry(3);
    $kachecknodes->set_sleeptime(10);
    $kachecknodes->set_nodelist($self->{nodelist});
    if ($kachecknodes->run()!=0) 
    { 
	$message->message(2,"check @mcattype node not found... (".$self->{nodelist}->get_str().")"); 
	return 0; 
    }
    return 1;
}

sub checknodes_db()
{
    my $self=shift;
    my $ok=1;
    my $nodelist=$self->{nodelist};
    my $refnodelist=$nodelist->get_nodes();
    my @node_list=@$refnodelist;

    $db->connect();	
    foreach my $node (@node_list)
    {
	if (! ($db->get_nodestate($node->get_name(),"MCAT") eq "UP"))
	{
	    $message->message(1,"mcat service for node ".$node->get_name()." is not up"); 
	    $nodelist->del($node->get_ip());
	    $ok=0;
	}
    }
    $self->{nodelist}=$nodelist;
    $db->disconnect();
    return $ok;
}

sub check_envfile()
{
    my $self=shift;
    my $environment=$self->{environment};
    my $ok=1;

    if (($environment->get("deploytype") eq "windows") &&
	(! $environment->is_set("windowsdirectory")))
    { $message->message(2,"Your environment file doesn't contain windowsdirectory"); $ok=0;    }
    
    if ($environment->get("deploytype") eq "linux" &&
	(! $environment->is_set("postinstall") ) )
	{ $message->message(2,"Your environment file doesn't contain postinstall"); $ok=0;    }

    if ($environment->get("deploytype") eq "solaris" &&
	(! $environment->is_set("postinstall") ) )
    { $message->message(2,"Your environment file doesn't contain postinstall"); $ok=0;    }
    
    if (! $ok) { $message->message(2,"envfile error : ".$environment->get_name()); }
    return $ok;
}


sub check_disk_partnumber()
{
    my $self=shift;
    my $ok=1;
    if (! $self->{disknumber})   { $message->missing_cmdline(2,"disknumber needed"); $ok=0; }
    if (! $self->{partnumber})   { $message->missing_cmdline(2,"partnumber needed"); $ok=0; }

    #Backward compatibility
    if ($self->{partnumber} =~ /^[hs]d[a-d](\d+)$/)
    { $self->{partnumber}=$1; }

    if (! ($self->{disknumber} =~ /^\d+$/)) { $message->missing_cmdline(2,"partnumber is a number begining at 1"); $ok=0; }
    if (! ($self->{partnumber} =~ /^\d+$/)) { $message->missing_cmdline(2,"partnumber is a number begining at 1"); $ok=0; }    
    return $ok;
}

sub check_necessary_right($)
{
    my $self=shift;
    my $righttocheck=shift;
    my $ok=1;
    if (!$self->check_rights($righttocheck))
    { $message->notenough_right(2,"user:".$self->{sudo_user}." right:".$righttocheck." node:".$self->{nodelist}->get_str()); $ok=0; }
    return $ok;
}

sub check_rights($)
{
    my $self=shift;
    my $righttocheck=shift;
    my $nodelist=$self->{nodelist};
    my $ok=0;
    my $karights=libkadeploy2::karights::new();

    $self->{sudo_user}=libkadeploy2::sudo::get_sudo_user();
    if (! $self->{sudo_user}) { $self->{sudo_user}=libkadeploy2::sudo::get_user(); }

    if ( $self->{sudo_user} eq "root" ||
	 $self->{sudo_user} eq $conf->get("deploy_user")
	 )
    {
	$ok=1;
    }

    $karights->set_nodelist($nodelist);
    if ($karights->check_rights($righttocheck))
    {
	$ok=1;
    }	
    return $ok;
}


sub partitionnodes()
{
    my $self=shift;
    my $kapart;
    my $environment=$self->{environment};
    my $ostype=$environment->get("deploytype");
    $kapart=libkadeploy2::kapart::new();
    $kapart->set_login($self->{envlogin});
    $kapart->set_envname($self->{envname});
    $kapart->run();

    if    ($ostype eq "linux")   { return $self->partitionlinuxnodes(); }
    elsif ($ostype eq "dd")      { return $self->partitionlinuxnodes(); }
    elsif ($ostype eq "windows") { return $self->partitionlinuxnodes(); }
    elsif ($ostype eq "solaris") { return $self->partitionsolarisnodes(); }
    else                         { return 0; }
}

sub partitionlinuxnodes()
{
    my $self=shift;
    my $cmd;
    my $command;
    
    my $kapart=libkadeploy2::kapart::new();
    $kapart->set_nodelist($self->{nodelist});
    $kapart->set_dofdisk();
    $kapart->set_ostype("linux");
    $kapart->set_partitionfile($self->{partitionfile});
    $kapart->set_disknumber($self->{disknumber});
    if ($kapart->run())
    { return 1; }
    else
    { return 0; }
}

sub partitionsolarisnodes()
{
    my $self=shift;
    my $cmd;
    my $command;
    
    my $kapart;

    $kapart=libkadeploy2::kapart::new();
    $kapart->set_nodelist($self->{nodelist});
    $kapart->set_dofdisk();
    $kapart->set_ostype("solaris");
    $kapart->set_partitionfile($self->{partitionfile});
    $kapart->set_disknumber($self->{disknumber});
    if ($kapart->run())
    { return 1; }
    else
    { return 0; }
}



sub deploy($)
{
    my $self=shift;
    my $deploytype=shift;
    my $deploy;
    my @node_list;
    @node_list=$self->{nodelist}->get_nodes();


    if    ($deploytype eq "linux")
    { 	$deploy=libkadeploy2::deploymethod::linux::new();        }
    elsif ($deploytype eq "dd")
    {   $deploy=libkadeploy2::deploymethod::dd::new();           }
    elsif ($deploytype eq "windows")  
    {   $deploy=libkadeploy2::deploymethod::windows::new();      }
    elsif ($deploytype eq "solaris")  
    {   $deploy=libkadeploy2::deploymethod::solaris::new();      }
    elsif ($deploytype eq "nfsrootlinux")  
    {   $deploy=libkadeploy2::deploymethod::nfsrootlinux::new(); }
    else
    {   $message->message(2,"deploymethod not found... kadeployenv::deploy($)"); exit 1;    }
    
    $deploy->set_nodelist($self->{nodelist});
    $deploy->set_partnumber($self->{partnumber});
    $deploy->set_disknumber($self->{disknumber});
    $deploy->set_envfile($self->{envfile});
    $deploy->set_partitionfile($self->{partitionfile});
    $deploy->set_connector($self->{connector});
    if ($self->{debug})   { $deploy->set_debug(); }
    if ($self->{verbose}) { $deploy->set_verbose(); }

    $message->message(0,"Begin deployment ".$deploytype." with ".$self->{nodelist}->get_str());
    if ($deploy->run()==0) { return 1; }
}


sub confsetuppxe($)
{
    my $self=shift;
    my $deploytype=shift;
    
    my $ok=1;

    my $environment=$self->{environment};

    my $pxetype;

    if ($environment->is_set("bootfromdisk") &&
	$environment->is_set("bootfromtftp"))
    {
	$message->message(2,"bootfromdisk & bootfromtftp mutualy exclusive !!!");
	return 0;
    }    
    elsif (
	   $environment->is_set("bootfromdisk") &&
	   $environment->get("bootfromdisk") eq "yes" 
	   )
    {
	$self->set_bootfromdisk();
    }
    elsif (
	   $environment->is_set("bootfromnetwork") &&
	   $environment->get("bootfromnetwork") eq "yes" 
	   )
    {
	$self->set_bootfromnetwork();
    }

    if ($environment->is_set("pxetype"))
    {
	$pxetype=$environment->get("pxetype");
	$self->set_pxetype($pxetype);
	$deploytype=$pxetype;
    }

    
    if ($deploytype =~ /^nfsroot.+/) 
    { 
	$self->set_deploytype($deploytype);
	$self->set_pxetype($deploytype);
    }
    elsif ($deploytype =~ /^pxe.+/)
    {
	$self->set_deploytype($deploytype);
	$self->set_pxetype($deploytype);
    }
    else
    {       
	$message->message(2,"deploytype = $deploytype pxetype = $pxetype (kadeployenv::confsetuppxe)");
	$ok=0;
    }
    return $ok;
}


sub setuppxe()
{
    my $self=shift;
    my $command;
    my $deploytype=$self->{environment}->get("deploytype");
    my $nodelist=$self->{nodelist};
    my $pxetype=$self->{pxetype};
    my $disknumbercmdline=$self->{disknumbercmdline};
    my $partnumbercmdline=$self->{partnumbercmdline};
    my $slicenumbercmdline=$self->{slicenumbercmdline};
    my $environment=$self->{environment};
    my $verbose=$self->{verbose};
    my $bootfrom="";
    my $cmd;

    if ($self->{bootfromnetwork})
    { $bootfrom=" --fromtftp"; }
    elsif ($self->{bootfromdisk})
    { $bootfrom=" --fromdisk"; }
    else { $message->message(2,"Internal error bootfrom unknow ??"); return 0; }
    
    $cmd="$kapxe ".$nodelist->get_cmdline().
	" --type ".$pxetype;


    if ($disknumbercmdline)
    { 	$cmd.=" --disknumber $disknumbercmdline"; }
    elsif ($environment->is_set("disknumber"))
    {   $cmd.=" --disknumber ".$environment->get("disknumber"); }

    if ($partnumbercmdline)
    { 	$cmd.=" --partnumber $partnumbercmdline"; }
    elsif ($environment->is_set("partnumber"))
    {   $cmd.=" --partnumber ".$environment->get("partnumber"); }

    if ($slicenumbercmdline)
    {   $cmd.=" --slicenumber $slicenumbercmdline"; }
    elsif ($environment->is_set("slicenumber"))
    {   $cmd.=" --slicenumber ".$environment->get("slicenumber"); }

    if ($environment->is_set("kernel"))
    { $cmd.=" --kernel ".$environment->get("kernel"); }
      
    if ($environment->is_set("initrd"))
    { $cmd.=" --initrd ".$environment->get("initrd"); }

    if ($environment->is_set("module"))
    { $cmd.=" --module ".$environment->get("module"); }

    if ($environment->is_set("kernelparams"))
    { $cmd.=" --kernelparams ".$environment->get("kernelparams"); }

    if ($environment->is_set("windowsdirectory"))
    { $cmd.=" --windowsdirectory ".$environment->get("windowsdirectory");     }
    
    $cmd.=" $bootfrom";
#    print $cmd;
    if ($verbose) 
    { 
	$cmd.=" -v";
 	$message->message(-1,"Launching $cmd");
    }

    
    $message->message(-1,"Setup PXE ".$nodelist->get_str());
    $command=libkadeploy2::command::new();
    $command->set_command($cmd);
    $command->set_timeout(50);
    if ($verbose) { $command->set_verbose(); }
    $command->exec();
    return $command->get_status();
}

1;
