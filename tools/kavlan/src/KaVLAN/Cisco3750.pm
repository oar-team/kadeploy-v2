#!/usr/bin/perl -w

##########################################################################################
# Specific file for the Cisco3750
# author       : Nicolas Niclausse
# date         : 29/07/2008
# note         :
##########################################################################################

package KaVLAN::Cisco3750;
use KaVLAN::Switch;
use KaVLAN::Cisco;
@ISA = ("KaVLAN::Cisco");

use strict;
use warnings;
use Data::Dumper;

sub new {
    my ($pkg)= @_;
    my $self = bless KaVLAN::Cisco->new("Cisco3750"),$pkg;
    return $self;
}


1;
