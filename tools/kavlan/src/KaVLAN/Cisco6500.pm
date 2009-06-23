#!/usr/bin/perl -w

##########################################################################################
# Specific file for the Cisco6500
# author       : Nicolas Niclausse
# date         : 29/07/2008
# note         :
##########################################################################################

package KaVLAN::Cisco6500;
use KaVLAN::Switch;
use KaVLAN::Cisco;
@ISA = ("KaVLAN::Cisco");

use strict;
use warnings;
use Data::Dumper;
our $CISCO_PORT_IFINDEX =".1.3.6.1.2.1.31.1.1.1.1";

sub new {
    my ($pkg)= @_;
    my $self = bless KaVLAN::Cisco->new("Cisco6500"),$pkg;
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
    my $self = shift;
    #Check arguments
    my ($vlanName,$switchSession)=@_;
    if(not defined $vlanName or not defined $switchSession){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }
    my %res;

    # Get port informations
    &const::debug("Getting ports affected");

    #Retrieve the vlan number
    my @vlanNumber;
    my $realVlanName = ($vlanName eq $const::DEFAULT_NAME) ? $const::VLAN_DEFAULT_NAME : $const::MODIFY_NAME_KAVLAN.$vlanName;
    @vlanNumber= $self->getVlanNumber($realVlanName,$switchSession);
    if($#vlanNumber == -1){
        die "ERROR : There is no vlan under this name";
    }
    my $untag = new SNMP::VarList([$self->{CISCO_LIST_UNTAG}]);
    foreach my $i ($switchSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$untag)) {
        foreach my $j (@ {$i}) {
            if ($j->[2] == $vlanNumber[0] and  $j->[0] =~ /(\d+)\.(\d+)$/) {
                my $port = "Gi$1/$2";
                push @{$res{"UNTAGGED"}}, $port;
            }
        }
    }
    ## FIXME: handle tagged port
    &const::verbose("TAGGED vlan not implemented");
    return \%res;
}

sub getPortIfIndex {
    my $self = shift;
    my ($port,$switchSession) = @_;
    if ($port =~ m@Gi(\d+)/(\d+)@) {
        my $untag =new SNMP::VarList([$CISCO_PORT_IFINDEX]);
        foreach my $i ($switchSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$untag)) {
            foreach my $j (@ {$i}) {
                if ($j->[2] eq $port) {
                    my $ifIndex = $j->[1];
                    &const::verbose("ifindex of port $port is $ifIndex");
                    return $ifIndex;
                }
            }
        }
        die "port index not found ($port)";
    } else {
        die "bad port format: $port";
    }
}


1;
