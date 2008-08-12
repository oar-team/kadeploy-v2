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

package KaVLAN::Foundry;
use KaVLAN::Switch;
@ISA = ("KaVLAN::Switch");

use strict;
use warnings;
use Data::Dumper;

# main OIDs
my $FOUNDRY_VLAN_NAME = ".1.3.6.1.2.1.17.7.1.4.3.1.1";
my $FOUNDRY_TAG       = "";
my $FOUNDRY_IP        = ".1.3.6.1.4.1.1991.1.2.2.18.1.2"; # only used for router ?
my $FOUNDRY_MASK      = ".1.3.6.1.4.1.1991.1.2.2.18.1.3"; # only used for router ?

my $MAX_PORTS=1000;

# specific OIDs
my $FOUNDRY_LIST_PORT = ".1.3.6.1.2.1.31.1.2.1.3"; # FIXME
my $FOUNDRY_LIST_UNTAG= ".1.3.6.1.4.1.1991.1.1.3.3.5.1.24"; # FIXME
my $FOUNDRY_PORT_IFINDEX = ".1.3.6.1.2.1.2.2.1.2"; # FIXME


#.1.3.6.1.2.1.2.2.1.2.524 = STRING: GigabitEthernet9/12

# grep 524 ~/fastiron.log  | grep 188
# .1.3.6.1.2.1.17.7.1.2.2.1.2.188.0.20.79.120.199.48 = INTEGER: 524
# .1.3.6.1.2.1.17.7.1.4.5.1.1.524 = Gauge32: 188
# .1.3.6.1.4.1.1991.1.1.3.2.6.1.1.188.524 = INTEGER: 188
# .1.3.6.1.4.1.1991.1.1.3.2.6.1.2.188.524 = INTEGER: 524
# .1.3.6.1.4.1.1991.1.1.3.2.6.1.3.188.524 = INTEGER: 2

# vlan of port 524: snSwIfVlanId
#.1.3.6.1.4.1.1991.1.1.3.3.5.1.24.524 = INTEGER: 188

# name if port 524:
#.1.3.6.1.2.1.2.2.1.2.524 = STRING: GigabitEthernet9/12

sub new {
    my ($pkg)= @_;
    my $self = bless KaVLAN::Switch->new("Foundry",$FOUNDRY_VLAN_NAME, $FOUNDRY_IP, $FOUNDRY_MASK, $FOUNDRY_TAG),$pkg;
    return $self;
}

##########################################################################################
# Get the IP Configuration of a vlan 
# arg : Integer -> the number of the vlan on the routeur session 
#       Session -> a session on which we can get the IP address
# ret : String -> the IP configuration 'IP/MASK'
# rmq :
##########################################################################################
sub getIPConfiguration {
    # no direct way with foundry; must get the vlan index first
    warn "getIPConfiguration not implemented";
    return;
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
     my $untag =new SNMP::VarList([$FOUNDRY_LIST_UNTAG]);
     foreach my $i ($switchSession->bulkwalk(0,$MAX_PORTS,$untag)) {
         foreach my $j (@ {$i}) {
             if ($j->[2] == $vlanNumber[0] and  $j->[0] =~ /(\d+)$/) {
                 my $port = &getPortFromIndex($1,$switchSession);
                 &const::verbose("port $port is in vlan  $vlanNumber[0]");
                 push @{$res{"UNTAGGED"}}, $port;
             }
         }
     }
    ## FIXME: handle tagged port
    warn "TAGGED vlan not implemented";

    $const::FUNC_NAME=$OLD_FUNC_NAME;
    return \%res;
}

sub getPortFromIndex {
    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="getPortFromIndex";
    &const::verbose();
    my $ifIndex;
    my ($index,$switchSession) = @_;
    my $allports =new SNMP::VarList([$FOUNDRY_PORT_IFINDEX]);
    my $port;
    foreach my $i ($switchSession->bulkwalk(0,$MAX_PORTS,$allports)) {
        foreach my $j (@ {$i}) {
            if ($j->[1] =~ /$index$/) {
                $port = $j->[2];
                $port =~ s/\d*\D+(\d+\/\d+)/$1/;
            }
        }
    }
    $const::FUNC_NAME=$OLD_FUNC_NAME;
    return $port;
}

sub getPortIfIndex {
    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="getPortIfIndex";
    &const::verbose();
    my $ifIndex;
    my ($port,$switchSession) = @_;
    if ($port =~ m@(\d+)/(\d+)@) {
        my $allports =new SNMP::VarList([$FOUNDRY_PORT_IFINDEX]);
        foreach my $i ($switchSession->bulkwalk(0,$MAX_PORTS,$allports)) {
            foreach my $j (@ {$i}) {
                Dumper($j);
                if ($j->[2] =~ /$port$/) {
                    my $ifIndex = $j->[0];
                    &const::verbose("ifindex of port $port is $ifIndex");
                }
            }
        }
        $const::FUNC_NAME=$OLD_FUNC_NAME;
        return $ifIndex;
    } else {
        die "bad port format: $port";
    }
}

1;
