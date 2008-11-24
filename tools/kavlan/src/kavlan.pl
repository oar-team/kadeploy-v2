#!/usr/bin/perl -w -I./src/ -I/usr/local/bin/kavlan

##########################################################################################
# KAVLAN 
# author       : Nicolas Niclausse
# date         : 02/10/2008
# note         :
##########################################################################################

package kavlan2;

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

my $OAR_PROPERTIES=$ENV{'OAR_RESOURCE_PROPERTIES_FILE'};
my $OAR_NODEFILE=$ENV{'OAR_NODEFILE'};

#Verify that there is at least one argument
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
# MANAGE ARGUMENTS
#--------------------------

# TODO: rights management

if ($options{"r"}) {   # get-network-range
    # TODO
    warn "get-network-range not implemented";
    exit 0;
} elsif ($options{"g"}) { # get-network-gateway
    # TODO
    warn "get-network-gateway not implemented";
    exit 0;
} elsif ($options{"d"} ){ # disable dhcp server for the given vlan
    # TODO
    warn "disable dhcp server: not implemented";
} elsif ($options{"s"} ){ # set vlan for given nodes
    my @nodes;
    my $VLAN;
    if  ($options{'i'}) { # vlan id is set
        $VLAN  = &get_vlan($options{"i"});
        @nodes = get_nodes($options{"f"}, $options{"m"});
        foreach my $node (@nodes) {
            my ($port,$switchName) = KaVLAN::Config::getPortNumber($node,$site->{"Name"});
            if ($port eq -1) { die "ERROR : Node $node not present in the configuration"; }
            my $indiceSwitch = &KaVLAN::Config::getSwitchIdByName($switchName,$switch);
            if($indiceSwitch==-1) {die "ERROR : There is no switch under this name";}
            if(&KaVLAN::Config::canModifyPort($node,$indiceSwitch,$switch)==0){
                my $otherMode;
                $otherMode=1 if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl");
                &vlan::addUntaggedPort($VLAN,$port,$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                die "ERROR : you can't modify this port";
            }
        }
    } elsif ($options{'j'}) {
        # use OAR job id to get the nodes
    } elsif ($OAR_NODEFILE) {
        # use OAR nodefile
        print "use oar nodefile: $OAR_NODEFILE\n";
        @nodes = get_nodes($OAR_NODEFILE, "");
    } else {
        print "No nodes specified: use -m, -f or -j\n";
        exit 1;
    }
} elsif ($options{"V"} ){ # get vlan id of job
    # TODO
    warn "get vlan_id: not implemented";
} elsif ($options{"l"} ){ # get node list of job
    # TODO
    warn "get node list: not implemented";
} else {
    die "no action specified, abort";
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
                push @nodelist, $1;
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
    if ($#nodelist >=0) {
        &const::verbose("OK, found ".($#nodelist+1)." node(s):\n".Dumper(@nodelist));
        return @nodelist;
    } else {
        die "no nodes found" ;
    }
}

sub check_node_name {
    my $nodename = shift;
    # node-XX.site.grid5000.fr or node-xx-ethXX.site.grid5000.fr
    return $nodename =~ m/^\w+-\d+(-\w+)?(\.\w+\.\w+\.\w+)?$/;
}

# check vlan_id parameter
sub get_vlan {
    my $vlan_id = shift;

    die "no vlan_id " unless $vlan_id;
    if ($vlan_id =~ m/default/i) {
        return $const::DEFAULT_NAME;
    } elsif ($vlan_id =~ m/^\d+$/)  {
        return $vlan_id if ($vlan_id >= 1 and $vlan_id <= $const::VLAN_MAX_ID);
    };
    die "abort: bad VLAN id ($vlan_id)";
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
       -h|--help                     print this help
       -v|--verbose                  verbose mode\n";
    exit $status;

}



