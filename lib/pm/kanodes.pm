package libkadeploy2::kanodes;

use Getopt::Long;
use libkadeploy2::deploy_iolib;
use libkadeploy2::conflib;
use libkadeploy2::nodesfile;
use libkadeploy2::disk;
use libkadeploy2::node;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::nodelist;

use strict;
use warnings;

sub loadnodesfile($);
sub loadpartitionfile($);
sub updatedeployedtable();
sub usage();
sub listnodes();
sub delpartitiontable();
sub listpartition($);
sub nodetoadd($);
sub nodetodel($);

my $message=libkadeploy2::message::new();
my $errorhandler=libkadeploy2::errorhandler::new();


my $kadeployconfdir="/etc/kadeploy";
my $nodesdir="$kadeployconfdir/nodes";
my $defaultpartitionfile=$kadeployconfdir."/clusterpartition.conf";
my $partitionfilecmdline="";

my @hostlist;	     
my $help;
my $listnode;
my $listpartition;
my $iwanthelp=1;
my $add;
my $del;
my $disk_id;
my @host_id_list = ();
my @part_env_id_list = ();
my $ok=1;
my $nodename;
my $create;

################################################################################

sub new()
{
    my $self=
    {
	add           => 0,
	del           => 0,
	create        => 0,
	listpartition => 0,
	listnode      => 0,
	help          => 0,
	
	partitionfile => "",	
	nodelist      => 0,
	verbose       => 0,
	timeout       => 0,
    };
    bless $self;
    return $self;
}


sub run()
{
    my $self=shift;
    my $retcode=0; 
    my $nodelist=$self->{nodelist};
    my $node;
    my $refnodelist;
    my @node_list;

    if ($nodelist)
    {
	$refnodelist=$nodelist->get_nodes();
	@node_list=@$refnodelist;
    }

    if (! $self->check_options()) 
    { 
	$errorhandler->exit_failto("check options");
    }

    if ($self->{add})
    {	
	$retcode=0;
	if (@node_list)
	{
	    foreach $node (@node_list)
	    {
		if ($self->nodetoadd($node->get_name())==0) { $retcode=1; }
	    }
	}
	else
	{
	    $message->missing_cmdline(2,"nodes");
	    $retcode=1;
	}
	return $retcode;
    }    
    elsif ($self->{create})
    {
	$retcode=0;
	if (! $self->create())  { $retcode=1; }
    }
    elsif ($self->{del})
    {
	$retcode=0;
	if (@node_list)
	{
	    
	    foreach $node (@node_list)
	    {	    
		if (nodetodel($node->get_name())==0)
		{
		    $retcode=1;
		}
	    }
	}
	else
	{
	    $message->missing_node_cmdline(2);
	    $retcode=1;
	}
	return $retcode;
    }    
    elsif ($self->{listnode})
    {
	$retcode=0;
	$self->listnodes();
	return $retcode;
    }   
    elsif ($self->{listpartition})
    {
	$retcode=0;
	if (@node_list)
	{
	    foreach $node (@node_list)
	    {
		$self->listpartition($node->get_name());
	    }
	}
	else
	{
	    $message->missing_node_cmdline(2);
	    $retcode=1;
	}
	return $retcode;
    }
    return $retcode;
}

sub get_options_cmdline()
{
    my $self=shift;

    my $ref_node_list;
    my $getopt=0;
    my @node_list;

    my $help=0;
    my $add=0;
    my $del=0;
    my $create=0;
    my $listnode=0;
    my $listpartition=0;
    

    $getopt=GetOptions('m=s'             => \@node_list,
		       'machine=s'       => \@node_list,
		       'h!'              => \$help,
		       'help!'           => \$help,
		       'add!'            => \$add,
		       'del!'            => \$del,
		       'create!'         => \$create,
		       'listnode!'       => \$listnode,
		       'listpartition!'  => \$listpartition,
		       'partitionfile=s' => \$partitionfilecmdline,
		       );
    if (! $getopt) { $errorhandler->wrong_parameters_commandline(); }

    if (@node_list)    
    { 
	$self->{nodelist}=libkadeploy2::nodelist::new(); 
	$self->{nodelist}->loadlist(\@node_list);	
    }

    $self->{add}=$add;
    $self->{del}=$del;
    $self->{create}=$create;

    $self->{listnode}=$listnode;
    $self->{listpartition}=$listpartition;
    $self->{partitionfile}=$partitionfilecmdline;
    $self->{help}=$help;    

}

sub check_options()
{
    my $self=shift;
    my $ok=1;
    if ($self->{help}) { $message->kanodes_help(); exit 0; }
    return $ok;
}


sub nodeexist($)
{
    my $self=shift;
    my $nodename=shift;
    my $ok=0;
    my $db;

    $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    if ($db->node_name_exist($nodename)) {$ok=1;}
    else {$ok=0;}
    $db->disconnect();
    return $ok;
}

sub nodetoadd($)
{
    my $self=shift;
    my $nodename=shift;
    $iwanthelp=0;
    my $disk;
    my $node;
    my $base;
    my $i;
    my $nodenamedir="$nodesdir/$nodename";
    my $nodefile="$nodenamedir/net";
    my $diskfileprefix="$nodenamedir/disk";
    my $partitionfileprefix="$nodenamedir/partition.conf";
    my $partitionfile="";
    my $errpartitionfile="";
    my $diskfile;
    my $ok=1;
    my $diskid;
    my $db;


    if (! -d $nodenamedir)   { $message->dirnotfound(2,$nodenamedir); return 0; }
    if (! -e $nodefile)      { $message->filenotfound(2,$nodefile);   return 0; }


    $node=libkadeploy2::node::new();
    $disk=libkadeploy2::disk::new();

    $message->loadingfile(0,$nodefile);

    if ($node->loadfile($nodefile))
    { 	
	$message->loadingfileDone(0,$nodefile);
	$node->addtodb();     
    }
    else
    { 
	$message->loadingfilefailed(2,$nodefile);
	$ok=0; 
    }      
    
    for ($i=1;$i<5;$i++)
    {
	$diskfile="$diskfileprefix-$i";
	$partitionfile="$partitionfileprefix-$i";
	$message->statfile(0,$diskfile);
	if (! -e $diskfile && $i==1) 
	{ 
	    $message->filenotfound(2,$diskfile);
	    return 0; 
	}
	if (-e $diskfile)
	{
	    $message->message(-1,"Checking $diskfile");
	    if (! $disk->loaddisksettingfile($diskfile)) 
	    { $message->message(2,"with diskfile $diskfile"); return 0; }

	    if (-e $partitionfilecmdline)
	    {
		if (! $disk->loadpartitionfile($partitionfilecmdline)) 
		{ $errpartitionfile=$partitionfilecmdline; $ok=0; }
	    }
	    elsif (-e "$partitionfileprefix-$i")
	    {
		if (! $disk->loadpartitionfile($partitionfile)) 
		{ $errpartitionfile=$partitionfile; $ok=0; }
	    }
	    elsif (-e $defaultpartitionfile)
	    {
		if (! $disk->loadpartitionfile($defaultpartitionfile)) 
		{ $errpartitionfile=$defaultpartitionfile; $ok=0; }
	    }
	    if (! $ok) { $message->message(2,"with $partitionfile $errpartitionfile"); return $ok; }

	    $db = libkadeploy2::deploy_iolib::new();
	    $db->connect();		

	    my $nodeid=$db->node_name_to_id($nodename);
	    my $diskid=$db->get_diskid_from_nodeid_disknumber($nodeid,$i);
	    if ($nodeid && $diskid)
	    {
		$message->message(0,"delete partition from $nodename");
		$db->del_partition_from_diskid($diskid);
		$ok=1;
	    }
	    if (! $disk->addtodb($nodename,$i)) { $ok=0;}
	    $db->disconnect();
	}
    }
    

    if ($ok)
    {
	updatedeployedtable(); 
	return $ok;
    }
    else
    {
	$message->message(2,"Check your configuration files...");
	return $ok;
    }

}


sub create()
{
    my $self=shift;
    my $ok=1;
    my $nodename;
    my $nodeip;
    my $nodemac;
    my $nodehdinterface;
    my $nodehdsize;
    my $nodedir;
    my $stepok=0;
    my $domkdir=0;
    my $netfile;
    my $diskfile;

    while ($stepok==0)
    {
	print "node name:";
	$nodename=<STDIN>;
	chomp($nodename);
	if ($nodename=~/[a-zA-Z\.\-]+/) { $stepok=1; }
    }

    $nodedir="/etc/kadeploy/nodes/$nodename";

    if (-d $nodedir)
    {
	$message->message(1,"node already exist in configuration files");
    }
    else
    {
	$domkdir=1;
    }
    
    $stepok=0;
    while ($stepok==0)
    {
	print "node ip  :";
	$nodeip=<STDIN>;
	chomp($nodeip);
	if ($nodeip =~ /\d+\.\d+\.\d+\.\d+/)
	{ $stepok=1; }
    }

    $stepok=0;
    while ($stepok==0)
    {
	print "node mac :";
	$nodemac=<STDIN>;
	chomp($nodemac);
	if ($nodemac =~ /..:..:..:..:..:../)
	{ $stepok=1; }
    }


    $stepok=0;
    while ($stepok==0)
    {
	print "node hard disk interface (ide|scsi|sata) :";
	$nodehdinterface=<STDIN>;
	chomp($nodehdinterface);
	if ($nodehdinterface =~ /ide|scsi|sata/)
	{ $stepok=1; }
    }

    $stepok=0;
    while ($stepok==0)
    {
	print "node hard disk size in gb :";
	$nodehdsize=<STDIN>;
	chomp($nodehdsize);
	if ($nodehdsize =~ /\d+/)
	{ $stepok=1; }
    }

    if ($domkdir) { $message->message(-1,"Creating dir $nodedir"); mkdir($nodedir); }
    $netfile=libkadeploy2::conflib::new("$nodedir/net",0);
    $netfile->set("name","$nodename");
    $netfile->set("ip","$nodeip");
    $netfile->set("mac","$nodemac");
    if ($netfile->write()) { $message->message(-1,"Writing $nodedir/net"); }
    else
    { 
	$message->message(2,"Can't write $nodedir/net");
	$ok=0; 
    }

    $diskfile=libkadeploy2::conflib::new("$nodedir/disk-1",0);
    $diskfile->set("interface","$nodehdinterface");
    $diskfile->set("size",$nodehdsize."g");
    $diskfile->set("mac","$nodemac");
    if ($diskfile->write()) { $message->message(-1,"Writing $nodedir/disk-1"); }
    else
    { 
	$message->message(2,"Can't write $nodedir/disk-1");
	$ok=0;
    }
    
    if ($ok)
    {
	$message->message(-1,"You now have to register it with flags --add ");
    }
    return $ok;
}


sub nodetodel($)
{
    my $nodename=shift;
    my $iwanthelp=0;
    my $db;
    my $diskid;
    my $nodeid;
    my $i;
    my $ok=0;

    $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    if ($db->node_name_to_id($nodename)==0)
    {
	$message->missing_node_db(2,$nodename);
	exit 1;
    }
    else
    {
	$nodeid=$db->node_name_to_id($nodename);
	for ($i=1;$i<5;$i++)
	{
	    $diskid=$db->get_diskid_from_nodeid_disknumber($nodeid,$i);
	    if ($diskid)
	    {
		$message->message(0,"delete partition from $nodename");
		$db->del_partition_from_diskid($diskid);
		$message->message(0,"delete disk from $nodename");
		$db->del_disk_from_id($diskid);
		$ok=1;
	    }
	    else
	    {
		$message->missing_disk_db(1,$nodename);
	    }
	}
	$message->message(0,"delete host $nodename");
	$db->del_node($nodename);
    }
    $db->disconnect();    
    return $ok;
}

sub listpartition($)
{
    my $self=shift;
    my $nodename=shift;
    my $partsize;
    my $partnumber=1;
    my $i;
    my $db = libkadeploy2::deploy_iolib::new();
    $db->connect();
    my $disk = libkadeploy2::disk::new();

    print $nodename;
    for ($i=1;$i<5;$i++)
    {
	if ($disk->get_fromdb($nodename,$i))
	{
	    print "#disk=$i\n";
	    $disk->print();
	    print "\n";
	    $disk=libkadeploy2::disk::new();
	}

    }
    $db->disconnect();
}

sub listnodes()
{
    my @nodelist;
    my $node;
    my $db = libkadeploy2::deploy_iolib::new();
    $db->connect();
    @nodelist=$db->list_node();
    print STDERR "node list\n";
    print STDERR "---------\n";
    foreach $node (@nodelist)
    {
	print "$node\n";
    }
    $db->disconnect();					  
}




sub updatedeployedtable()
{
    my $db = libkadeploy2::deploy_iolib::new();
    $db->connect();
    foreach my $host (@host_id_list)
    {
	foreach my $part_env (@part_env_id_list)
	{
	    $db->add_deploy(\$part_env,$disk_id,$host);
	  }
    }
    
    $db->disconnect();    
}


1;
