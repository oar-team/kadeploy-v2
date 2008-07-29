#!/usr/bin/perl -w

##########################################################################################
# Const file of kavlan 
# author       : Jérémie TISSERAND
# date         : 29/08/2006
# note         : 
##########################################################################################
# version      :
# modified     : 
# author       :
# modification :
##########################################################################################



package const;
@ISA = ("Exporter");



#For a better syntaxe
use strict;

#Version of kavlan
our $VERSION="1.0";

#Location of the configuration file
our $CONFIGURATION_FILE="/etc/kavlan/kavlan.conf";

#Location of the configuration file
our $PATH_TABLE_CORES="/etc/kavlan/";

#Default name of VLAN that kavlan can modify
our $DEFAULT_NAME_KAVLAN="KAVLAN_";

#Name reserved to define the default name
our $DEFAULT_NAME="DEFAULT";

#Default name of the vlan on the site
our $VLAN_DEFAULT_NAME;

#Name of vlan when kavlan have modify them we are using the USER env variable to put it in the name of the vlan
#in order to know which vlan this user have modified and allow other people not to change configuration of our vlan
our $MODIFY_NAME_KAVLAN="KAVLAN-";

#Maximum of vlan allowed by ieee on network appliances
our $IEEE_MAX_VLAN=4095;

#Activate the verbose mode
our $VERBOSE=0;
#Uses during the verbose mode to show the name of the function this variable have to be redefined in each function in order to override the variable of the caller function
our $FUNC_NAME="main";

##########################################################################################
# Verbose function 
# arg : ...
# ret : 
# rmq : print all the arguments and a '\n' if the verbose mode is activated
##########################################################################################
sub verbose(){

    my @args=@_;

    if($const::VERBOSE!=0){
        print $0."->".$const::FUNC_NAME."::";
        foreach my $item (@args){
            print $item;
    }

        print "\n";
    }
}

1;
