#!/usr/bin/perl -w -I./src/ -I/usr/local/bin/kavlan

##########################################################################################
# KAVLAN 
# author       : Nicolas Niclausse
# date         : 02/10/2008
# note         :
##########################################################################################

package kavlan;

use strict;
use Getopt::Long;

use SNMP;

use const;
use vlan;

use Data::Dumper;

use KaVLAN::Config;
use KaVLAN::summit;
use KaVLAN::hp3400cl;
use KaVLAN::Cisco3750;
use KaVLAN::Foundry;
use KaVLAN::RightsMgt;

my $OAR_PROPERTIES=$ENV{'OAR_RESOURCE_PROPERTIES_FILE'};
my $OAR_NODEFILE=$ENV{'OAR_NODEFILE'};
my $OARSTAT="oarstat"; # oarstat command
my $VLAN_PROPERTY_NAME="vlan"; # OAR property name of the VLAN ressource

# Verify that there is at least one argument
if($#ARGV < 0){
    &usage();
    exit(0);
}
&Getopt::Long::Configure("no_ignore_case");

my %options;
GetOptions(\%options,
        "r|get-network-range",
        "g|get-network-gateway",
        "d|disable-dhcp",
        "i|vlan_id=s",
        "l|get_nodelist",
        "V|get_vlan_id",
        "j|job_id=s",
        "f|filenode=s",
        "m|machine=s@",
        "s|set",
        "q|quiet",
        "h|help",
        "v|verbose");

&usage(0) if( $options{"h"});

#------------------------------
# PARSE THE CONFIGURATION FILE
#------------------------------
$const::CONFIGURATION_FILE = $options{"F"} if ($options{"F"});
$const::VERBOSE=1 if $options{"v"};

my ($site,$routeur,$switch) = KaVLAN::Config::parseConfigurationFile();

$const::VLAN_DEFAULT_NAME=$site->{"VlanDefaultName"};

#-----------------------------
# GET APPLIANCE CONFIGURATION
#-----------------------------

#Get the configurations informations of the appliances
my $routeurConfig;
my @switchConfig;

#Verifying if the -s option is activated to avoid loading router configuration
if(not defined $options{"s"}){
    if($routeur->{"Type"} eq "summit"){$routeurConfig = KaVLAN::summit->new();}
    elsif($routeur->{"Type"} eq "hp3400cl"){$routeurConfig =  KaVLAN::hp3400cl->new();}
    elsif($routeur->{"Type"} eq "Cisco3750"){$routeurConfig = KaVLAN::Cisco3750->new();}
    elsif($routeur->{"Type"} eq "Foundry"){$routeurConfig = KaVLAN::Foundry->new();}
    else{die "ERROR : The routeur type doesn't exist";}
}

#We are loading all the switch information
foreach my $i (0 .. $#{$switch}){
    if($switch->[$i]{"Type"} eq "summit"){
        $switchConfig[$i] = KaVLAN::summit->new();
    }
    elsif($switch->[$i]{"Type"} eq "hp3400cl"){
        $switchConfig[$i] =  KaVLAN::hp3400cl->new();
    }
    elsif($switch->[$i]{"Type"} eq "Cisco3750"){
        $switchConfig[$i] = KaVLAN::Cisco3750->new();
    }
    elsif($switch->[$i]{"Type"} eq "Foundry"){
        $switchConfig[$i] = KaVLAN::Foundry->new();
    }
    else{
        die "ERROR : The switch type doesn't exist";
    }
}

#---------------------------------------
# INITIALIZATION OF SNMP COMMUNICATIONS
#---------------------------------------

#SNMP informations
my $COMMUNITY = (defined $site->{"SNMPCommunity"}) ? $site->{"SNMPCommunity"} : "private";
my $routeurSession;
my @switchSession;

#Create the SNMP sessions
if(not defined $options{"s"}){
    $routeurSession= new SNMP::Session(DestHost => $routeur->{"IP"},
            Community => $COMMUNITY,
            Version => "2c",
            Timeout => 300000);
}
foreach my $i (0 .. $#{$switch}){
    print "establish session to remote SNMP server $switch->[$i]{IP}\n" if ($const::VERBOSE);
    $switchSession[$i]= new SNMP::Session(DestHost => $switch->[$i]{"IP"},
            Community => $COMMUNITY,
            Version => "2c",
            Timeout => 300000);
}

#--------------------------
# MANAGE DATABASE
#--------------------------
my $dbuser   = $site->{DbUser};
my $dbpasswd = $site->{DbPasswd};
my $dbhost   = $site->{DbHost};
my $dbname   = $site->{DbName};

die "Database not configured correctly, check your configuration file" unless ($dbuser and $dbpasswd and $dbhost and $dbname);
my $dbh = &KaVLAN::RightsMgt::connect($dbhost,$dbname,$dbuser,$dbpasswd);

#--------------------------
# MANAGE ARGUMENTS
#--------------------------

# TODO: unsecure, just for testing.
my $USER= $ENV{USER};

if ($options{"r"}) {   # get-network-range
    # TODO
    my $VLAN  = &get_vlan();
    die "get-network-range not implemented";
} elsif ($options{"g"}) { # get-network-gateway
    # TODO
    my $VLAN  = &get_vlan();
    die "get-network-gateway not implemented";
} elsif ($options{"d"} ){ # disable dhcp server for the given vlan
    # TODO
    my $VLAN  = &get_vlan();
    die "disable dhcp server: not implemented";
} elsif ($options{"s"} ){ # set vlan for given nodes
    my @nodes;
    my $VLAN  = &get_vlan();
    if  ($options{'i'}) { # vlan id is set
        @nodes = get_nodes($options{"f"}, $options{"m"});
    } elsif ($options{'j'}) {
        # use OAR job id to get the nodes
        @nodes = &get_nodes_from_oarjob($options{"j"});
    } elsif ($OAR_NODEFILE) { # no job or vlan id specified, look for OAR env. variables
        # use OAR nodefile
        print "use oar nodefile: $OAR_NODEFILE\n" unless $options{"quiet"};
        @nodes = get_nodes($OAR_NODEFILE, "");
    } else {
        print "No nodes specified: use -m, -f or -j\n";
        exit 1;
    };
    if (defined $VLAN) {
        &KaVLAN::Config::check_nodes_configuration(\@nodes,$site,$switch);
        unless (&KaVLAN::RightsMgt::check_rights_nodelist($dbh, $USER,\@nodes,$VLAN)) {
            die "No rights to change VLAN, abort" ;
        }
        &const::verbose("User $USER has enough rights to change the VLAN of all nodes, continue");
        foreach my $node (@nodes) {
            &set_vlan($node,$VLAN);
        }
    } else {
        die "No VLAN found, abort!";
    }
    print "all nodes are configured in the vlan $VLAN\n" unless $options{"quiet"};
} elsif ($options{"V"} ){ # get vlan id of job
    print &get_vlan_from_oar($options{'j'});
    print "\n";
} elsif ($options{"l"} ){ # get node list of job
    my @nodes;
    my @nodes_default; # node name in default vlan
    my $JOBID=$options{'j'};
    my $VLAN = &get_vlan_from_oar($JOBID);
    if ($JOBID) {
        @nodes_default = &get_nodes_from_oarjob($JOBID);
    } elsif ($OAR_NODEFILE) {
        @nodes_default = &get_nodes($OAR_NODEFILE, "");
    } else {
        die "get node list: no job specified, use -j";
    }
    die "no VLAN found" unless $VLAN;
    die "no nodes found" if ($#nodes_default < 0);
    # rewrite nodename: add -vlanX where X is the vlan ID
    @nodes = map { s/^(\w+-\d+)\./$1\-vlan$VLAN\./; $_ } @nodes_default;
    foreach (@nodes) {print "$_\n";};
} else {
    die "no action specified, abort";
}


&KaVLAN::RightsMgt::disconnect($dbh);


## -----------------------------------------------------------------------
## End of main script here -----------------------------------------------
## -----------------------------------------------------------------------

sub set_vlan {
    my $node = shift;
    my $VLAN = shift;
    my ($port,$switchName) = KaVLAN::Config::getPortNumber($node,$site->{"Name"});
    my $indiceSwitch = &KaVLAN::Config::getSwitchIdByName($switchName,$switch);

    # we have already checked before (in check_nodes_configuration
    # )that the indice is defined and we have rights to modify the
    # port, therefore, we can skip checks here
    my $otherMode;
    $otherMode=1 if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl");
    &vlan::addUntaggedPort($VLAN,$port,$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
    print " ... node $node changed to vlan KAVLAN-$VLAN\n"  unless $options{"quiet"};
}

sub get_vlan {
    my $VLAN;
    if  ($options{'i'}) { # vlan id is set
        $VLAN  = &check_vlan($options{"i"});
    } elsif ($options{'j'}) {
        # use OAR job id to get the nodes
        $VLAN  = &get_vlan_from_oar($options{"j"});
    } elsif ($OAR_NODEFILE) { # no job or vlan id specified, look for OAR env. variables
        # use OAR nodefile
        $VLAN  = &get_vlan_from_oar();
    } else {
        return undef;
    }
}

# returns vlan id of job; if jobid is undef, check OAR env. variables.
sub get_vlan_from_oar {
    my $jobid = shift;
    if ($jobid) {
        &const::verbose("try to get VLAN id from job $jobid");
        return &get_vlan_property("",$jobid);
    } elsif ($OAR_PROPERTIES) {
        &const::verbose("try to get VLAN id from OAR_PROPERTIES file");
        return &get_vlan_property($OAR_PROPERTIES,"");
    } else {
        die "no job specified, use -j";
    }
}


sub get_nodes_from_oarjob {
    my $JOBID = shift;

    my @nodes_default;

    if ($JOBID =~ m/^\d+$/) {
        &const::verbose("get nodes from oarstat: $OARSTAT -f -j $JOBID  ");
        open(OARSTAT, "$OARSTAT -f -j $JOBID |") or die "Error while running oarstat: $!";
        while (<OARSTAT>) {
            if  (/assigned_hostnames = (.*)$/) {
                @nodes_default= split(/\+/,$1);
            }
        };
        close(OARSTAT);
        return @nodes_default;
    } else {
        die "Wrong jobid given ($JOBID), abort!";
    }

}

# return: list of nodes, or die if empty nodelist
sub get_nodes {
    my $nodefile = shift;  # filename
    my $nodes    = shift;  # arrayref

    my @nodelist;
    if ($nodefile) {
        # open file, uniquify nodes
        open(NODEFILE, "uniq $nodefile|") or die "can't open nodefile ($nodefile), abort ! $!";
        while (<NODEFILE>) {
            chomp;
            if (&check_node_name($_)) {
                push @nodelist, $_;
            } else {
                warn "skip node $_";
            }
        }
        close(NODEFILE);
    }

    if ($nodes) {
        &const::verbose("read node list (-m )");
        my %seen = ();
        foreach my $elem ( @$nodes )
            {
                next unless &check_node_name($elem);
                next if $seen{ $elem }++;
                push @nodelist, $elem;
            }
    }
    return @nodelist;
}

# check if node name is valid (with or without domain)
# => node-XX.site.grid5000.fr or node-xx-ethXX.site.grid5000.fr
sub check_node_name {
    my $nodename = shift;
    return $nodename =~ m/^\w+-\d+(-\w+)?(\.\w+\.\w+\.\w+)?$/;
}

# check vlan_id parameter when given by the user
sub check_vlan {
    my $vlan_id = shift;

    die "no vlan_id " unless $vlan_id;
    if ($vlan_id =~ m/default/i) {
        return $const::DEFAULT_NAME;
    } elsif ($vlan_id =~ m/^\d+$/)  {
        return $vlan_id if ($vlan_id >= 1 and $vlan_id <= $const::VLAN_MAX_ID);
    };
    die "abort: bad VLAN id ($vlan_id)";
}

sub get_vlan_property {
    my $filename = shift;
    my $jobid    = shift;
    if ( $jobid > 0) {
        open(PROP, "$OARSTAT -p -j $jobid |") or die "can't start oarstat, abort ! $!";
    } elsif (-f $filename ) {
        open(PROP, "< $filename") or die "can't open $filename, abort ! $!";
    }
    while (<PROP>) {
        chomp;
        foreach my $prop (split /\s+\,\s+/) {
            if ($prop =~ m/$VLAN_PROPERTY_NAME\s+\=\s+\'(\w+)\'/) {
                &const::verbose("found vlan = $1");
                close(PROP);
                return $1;
            }
        }
    }
    close(PROP);
    die "Can't find VLAN from OAR properties, abort";
}

sub usage(){
    my $status= shift;
    $status=1 unless defined $status;
print "Version $const::VERSION
USAGE : $0 [options]
       -r|--get-network-range
       -g|--get-network-gateway
       -l|--get-nodelist
          --get-vlan-id              print VLAN ID of job (needs -j JOBID)
       -d|--disable-dhcp
       -i|--vlan_id <VLANID>
       -s                            set vlan for given node(s)
       -f|--filenode <NODEFILE>
       -j|--oar-jobid=XXXX
       -m|--machine <nodename>
       -q|--quiet                    quiet mode
       -h|--help                     print this help
       -v|--verbose                  verbose mode\n";
    exit $status;
}
