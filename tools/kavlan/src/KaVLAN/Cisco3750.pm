#!/usr/bin/perl -w

##########################################################################################
# Specific file for the Cisco3750
# author       : Nicolas Niclausse
# date         : 29/07/2008
# note         :
##########################################################################################
# version      :
# modified     :
# author       :
# modification :
##########################################################################################

package KaVLAN::Cisco3750;
use KaVLAN::Switch;
@ISA = ("KaVLAN::Switch");

use strict;
use warnings;
use Data::Dumper;

# Documentation: 
# How To Add, Modify, and Remove VLANs on a Catalyst Using SNMP:
# http://www.cisco.com/en/US/tech/tk648/tk362/technologies_tech_note09186a00801c6035.shtml
#
# see also:
# http://mediatools.cs.ucl.ac.uk/nets/hen/browser/hen_scripts/trunk/lib/hardware/switches/cisco.py?rev=638

my $CISCO_VLAN_NAME = ".1.3.6.1.4.1.9.9.46.1.3.1.1.4.1";
my $CISCO_TAG       = "";
my $CISCO_IP        = ".1.3.6.1.2.1.3.1.1.3"; # RFC1213-MIB::atNetAddress
my $CISCO_MASK      = ".1.3.6.1.2.1.4.20.1.3"; #IP-MIB::ipAdEntNetMask IpAddress
my $CISCO_LIST_PORT = ".1.3.6.1.2.1.31.1.2.1.3";
my $CISCO_LIST_UNTAG= ".1.3.6.1.4.1.9.5.1.9.3.1.3";
# vlans list :       .1.3.6.1.4.1.9.9.46.1.3.1.1.2 (vtpVlanState)
# .1.3.6.1.4.1.9.9.68.1.2.2.1.2 #vmvlan: untag vlan for every port
# .1.3.6.1.4.1.9.9.68.1.2.2.1.4 #vmvlans
# .1.3.6.1.4.1.9.9.46           #ciscoVtpMIB
# .1.3.6.1.4.1.9.9.46.1.6.1.1.5 # native vlan

sub new {
    my ($pkg)= @_;
    my $self = bless KaVLAN::Switch->new("Cisco3750",$CISCO_VLAN_NAME, $CISCO_IP, $CISCO_MASK, $CISCO_TAG),$pkg;
    return $self;
}

##########################################################################################
# Get the ports affected to a vlan 
# arg : String -> the vlan name
#       Session -> a switch session
# ret : hash table reference : -> "TAGGED" array containing the tagged ports
#                              -> "UNTAGGED" array containing the untagged ports
# rmq : The vlan have to be present on the switch
##########################################################################################
sub getPortsAffectedToVlan(){
    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="getPortsAffectedToVlan";
    &const::verbose();

    my %res;
    my $self = shift;
    #Check arguments
    my ($vlanName,$switchSession)=@_;
    if(not defined $vlanName or not defined $switchSession){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }

    #Get port informations
    &const::verbose("Getting ports affected");

    #Retrieve the vlan number
    my @vlanNumber;
    my $realVlanName = ($vlanName eq $const::DEFAULT_NAME) ? $const::VLAN_DEFAULT_NAME : $const::MODIFY_NAME_KAVLAN.$vlanName;
    @vlanNumber= $self->getVlanNumber($realVlanName,$switchSession);
    if($#vlanNumber == -1){
        die "ERROR : There is no vlan under this name";
    }
    my $untag =new SNMP::VarList([$CISCO_LIST_UNTAG]);
    foreach my $i ($switchSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$untag)) {
        foreach my $j (@ {$i}) {
            if ($j->[2] == $vlanNumber[0] and  $j->[0] =~ /(\d+)\.(\d+)$/) {
                my $port = "$1/0/$2";
                push @{$res{"UNTAGGED"}}, $port;
            }
        }
    }
    ## FIXME: handle tagged port
    warn "TAGGED vlan not implemented";

    $const::FUNC_NAME=$OLD_FUNC_NAME;
    return \%res;
}

1;
