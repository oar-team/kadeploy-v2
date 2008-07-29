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

# liste des vlans:   .1.3.6.1.4.1.9.9.46.1.3.1.1.2 (vtpVlanState)
my $CISCO_VLAN_NAME=".1.3.6.1.4.1.9.9.46.1.3.1.1.4.1";
my $CISCO_TAG="undef";
my $CISCO_IP=".1.3.6.1.2.1.3.1.1.3"; # RFC1213-MIB::atNetAddress
my $CISCO_MASK=".1.3.6.1.2.1.4.20.1.3"; #IP-MIB::ipAdEntNetMask IpAddress
my $CISCO_LIST_PORT=".1.3.6.1.2.1.31.1.2.1.3";

sub new {
    my ($pkg)= @_;
    my $self = bless KaVLAN::Switch->new("Cisco3750",$CISCO_VLAN_NAME, $CISCO_IP, $CISCO_MASK, $CISCO_TAG),$pkg;
    return $self;
}

1;
