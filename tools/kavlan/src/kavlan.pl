#!/usr/bin/perl -w -I./src/ -I/usr/local/bin/kavlan

##########################################################################################
# KAVLAN 
# author       : Jérémie TISSERAND
# date         : 29/08/2006
# note         : 
##########################################################################################
# version      :
# modified     : 
# author       :
# modification :
##########################################################################################

package kavlan;

#For a better syntaxe
use strict;
#Include functions to parse the command line
use Getopt::Long;

use SNMP;

use const;
use vlan;

use KaVLAN::Config qw[parseConfigurationFile];
use KaVLAN::summit;
use KaVLAN::hp3400cl;
use KaVLAN::Cisco3750;
use KaVLAN::Foundry;
#-----------------------
# PARSE ARGUMENTS
#-----------------------

#Verify that there is at least one argument
if($#ARGV < 0){
    &usage();
    exit(0);
}
&Getopt::Long::Configure("no_ignore_case");
#Retreive arguments
my %options;
GetOptions(\%options,
        "a|add=s",
        "d|del=s",
        "l|list:s",
        "i|information=s",
        "p|port=s",
        "t|tag=s",
        "u|untag=s",
        "r|remove=s",
        "z|zero=s",
        "s|switch=s",
        "F|config=s",
        "P|path=s",
        "h|help",
        "v|verbose");


if(defined $options{"h"}){
    &usage();
    exit 0;
}

#------------------------------
# PARSE THE CONFIGURATION FILE
#------------------------------
if(defined $options{"F"}){
    $const::CONFIGURATION_FILE = $options{"F"};
}

if(defined $options{"v"}){
    print "Verbose mode activated\n";
    $const::VERBOSE=1;
}

my ($site,$routeur,$switch) = &KaVLAN::Config::parseConfigurationFile();


$const::VLAN_DEFAULT_NAME=$site->{"VlanDefaultName"};

if(defined $options{"P"}){
    $const::PATH_TABLE_CORES = $options{"P"};
#We add the "/" at the end of the path if it is not present
    my $end = chop($const::PATH_TABLE_CORES);
    if($end ne "/"){
        $const::PATH_TABLE_CORES .= $end;
    }
    $const::PATH_TABLE_CORES .= "/";
}


#-----------------------------
# GET APPLIANCE CONFIGURATION
#-----------------------------

#Get the configurations informations of the appliances
my $routeurConfig;
my @switchConfig;
my $i;
my $indiceSwitch;

#Verifying if the -s option is activated to avoid loading routeur configuration
if(not defined $options{"s"}){
    if($routeur->{"Type"} eq "summit"){$routeurConfig = KaVLAN::summit->new();}
    elsif($routeur->{"Type"} eq "hp3400cl"){$routeurConfig =  KaVLAN::hp3400cl->new();}
    elsif($routeur->{"Type"} eq "Cisco3750"){$routeurConfig = KaVLAN::Cisco3750->new();}
    elsif($routeur->{"Type"} eq "Foundry"){$routeurConfig = KaVLAN::Foundry->new();}
    else{die "ERROR : The routeur type doesn't exist";}
}

#We are loading all the switch information
for($i = 0 ; $i <  ($#{$switch}+1) ; $i++){
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


#If the -s option is enable we verify if the name of the switch is present
if(defined $options{"s"}){
    $indiceSwitch = &getSwitchIdByName($options{"s"});
    print "indiceswitch: $indiceSwitch\n" if ($const::VERBOSE);
    if($indiceSwitch == -1){
        die "ERROR : There is no switch under this name";
    }
}
else{
    $indiceSwitch = 0;
}

#---------------------------------------
# INITIALISATION OF SNMP COMMUNICATIONS
#---------------------------------------

#SNMP informations
my $COMMUNITY; 
if(defined $site->{"SNMPCommunity"}){
    $COMMUNITY=$site->{"SNMPCommunity"};
}
else{
    $COMMUNITY="private";
}
my $routeurSession;
my @switchSession;

#Create the SNMP sessions
if(not defined $options{"s"}){
    $routeurSession= new SNMP::Session(DestHost => $routeur->{"IP"},
            Community => $COMMUNITY,
            Version => "2c",
            Timeout => 300000);
}
for($i = 0 ; $i <  ($#{$switch}+1) ; $i++){
    print "establish session to remote SNMP server $switch->[$i]{IP}\n" if ($const::VERBOSE);
    $switchSession[$i]= new SNMP::Session(DestHost => $switch->[$i]{"IP"},
            Community => $COMMUNITY,
            Version => "2c",
            Timeout => 300000);

}
#--------------------------
# MANAGE ARGUMENTS
#--------------------------

#TODO : Allow the access by name for machine instead of port access

#treatment loop
foreach (keys %options){

    if($_ eq "a"){
        if(defined $options{"a"} && $#ARGV==-1){
            if(defined $options{"s"}){
                &vlan::addVlanOnSwitch($options{"a"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch]);
            }
            else{
                &vlan::addVlanOnRouteur($options{"a"},$routeurSession,$routeurConfig);
                for($i = 0 ; $i <  ($#{$switch}+1) ; $i++){
                    if($switch->[$i]{"IP"} eq $routeur->{"IP"}){
                        &const::verbose("switch and routeur are the same appliance");
                    }
                    else{
                        &vlan::addVlanOnSwitch($options{"a"},$switchSession[$i],$switchConfig[$i]);
                    }
                }
            }
        }
        else{&usage();}
    }
    elsif($_ eq "d"){
        if(defined $options{"d"}&& $#ARGV==-1){
            if(defined $options{"s"}){
                my $otherMode;
                if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                &vlan::delVlanOnSwitch($options{"d"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                for($i = 0 ; $i <  ($#{$switch}+1) ; $i++){
                    my $otherMode;
                    if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                    &vlan::delVlanOnSwitch($options{"d"},$switchSession[$i],$switchConfig[$i],$otherMode);
                    if($switch->[$i]{"IP"} eq $routeur->{"IP"}){
                        &const::verbose("switch and routeur are the same appliance");
                    }
                    else{
                        &vlan::delVlanOnRouteur($options{"d"},$routeurSession,$routeurConfig);
                    }
                }
            }
        }
        else{&usage();}

    }
    elsif($_ eq "l"){
        my $listVar;
        if(defined $options{"l"}){$listVar = $options{"l"};}
        else{$listVar="";}
        if($#ARGV==-1){
            if(defined $options{"s"}){
                $switchConfig[$indiceSwitch]->listVlanOnSwitch($listVar,$switchSession[$indiceSwitch]);
            }
            else{
                $routeurConfig->listVlanOnRouteur($listVar,$routeurSession);
            }
        }
        else{&usage();}
    }
    elsif($_ eq "i"){
        if(defined $options{"i"}){
            if(defined $options{"s"}){
                &printPortsAffectedToVlan($options{"i"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$switch->[$indiceSwitch]{"Name"});
            }
            else{
                for($i = 0 ; $i <  ($#{$switch}+1) ; $i++){
                    print "SWITCH : ".$switch->[$i]{"Name"}."\n";
                    &printPortsAffectedToVlan($options{"i"},$switchSession[$i],$switchConfig[$i],$switch->[$i]{"Name"});
                }
            }
        }
        else{&usage();}


    }
    elsif($_ eq "p"){
        if(defined $options{"p"}&& $#ARGV==-1){
            my $switchName;
            if( &is_computer_name($options{"p"})){
                &const::verbose("The port given is a computer name");
                ($options{"p"},$switchName) = KaVLAN::Config::getPortNumber($options{"p"},$site->{"Name"});
                if($options{"p"} eq -1){die "ERROR : Computer not present in the list";}
                $indiceSwitch = &getSwitchIdByName($switchName);
                if($indiceSwitch==-1){die "ERROR : There is no switch under this name";}
            }
            if(defined $options{"s"}){
                &printPortInformation($options{"p"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch]);
            }
            else{
                &printPortInformation($options{"p"},$switchSession[0],$switchConfig[0]);
            }
        }
        else{&usage();}
    }
    elsif($_ eq "t"){
        if($#ARGV+1 == 1 && defined $options{"t"}){
            my $switchName;
            if( &is_computer_name($options{"t"})){
                &const::verbose("The port given is a computer name");
                ($options{"t"},$switchName) = KaVLAN::Config::getPortNumber($options{"t"},$site->{"Name"});
                if($options{"t"} == -1){die "ERROR : Computer not present in the list";}
                $indiceSwitch = &getSwitchIdByName($switchName);
                if($indiceSwitch==-1){die "ERROR : There is no switch under this name";}
            }
            if(&canModifyPort($options{"t"},$indiceSwitch,$switch)==0){
                my $otherMode;
                if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                &vlan::addTaggedPort($ARGV[0],$options{"t"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                die "ERROR : you can't modify this port";
            }

        }
        else{&usage();}
    }
    elsif($_ eq "u"){
        if($#ARGV+1 == 1 && defined $options{"u"}){
            my $switchName;
            if( &is_computer_name($options{"u"})){
                &const::verbose("The port given is a computer name");
                ($options{"u"},$switchName) = KaVLAN::Config::getPortNumber($options{"u"},$site->{"Name"});
                if($options{"u"} eq -1){die "ERROR : Computer not present in the list";}
                $indiceSwitch = &getSwitchIdByName($switchName);
                if($indiceSwitch==-1){die "ERROR : There is no switch under this name";}
            }
            if(&canModifyPort($options{"u"},$indiceSwitch,$switch)==0){
                my $otherMode;
                if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                &vlan::addUntaggedPort($ARGV[0],$options{"u"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                die "ERROR : you can't modify this port";
            }

        }
        else{&usage();}
    }
    elsif($_ eq "r"){
        if($#ARGV+1 == 1 && defined $options{"r"}){
            my $switchName;
            if( &is_computer_name($options{"r"})){
                &const::verbose("The port given is a computer name");
                ($options{"r"},$switchName) = KaVLAN::Config::getPortNumber($options{"r"},$site->{"Name"});
                if($options{"r"} == -1){die "ERROR : Computer not present in the list";}
                $indiceSwitch = &getSwitchIdByName($switchName);
                if($indiceSwitch==-1){die "ERROR : There is no switch under this name";}
            }
            if(&canModifyPort($options{"r"},$indiceSwitch,$switch)==0){
                my $otherMode;
                if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                &vlan::removePort($ARGV[0],$options{"r"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                die "ERROR : you can't modify this port";
            }

        }
        else{&usage();}
    }
    elsif($_ eq "z"){
        if(defined $options{"z"}&& $#ARGV==-1){
            my $switchName;
            if( &is_computer_name($options{"z"})){
                &const::verbose("The port given is a computer name");
                ($options{"z"},$switchName) = KaVLAN::Config::getPortNumber($options{"z"},$site->{"Name"});
                if($options{"z"} == -1){die "ERROR : Computer not present in the list";}
                $indiceSwitch = &getSwitchIdByName($switchName);
                if($indiceSwitch==-1){die "ERROR : There is no switch under this name";}
            }
            if(&canModifyPort($options{"z"},$indiceSwitch,$switch)==0){
                my $otherMode;
                if($switch->[$indiceSwitch]{"Type"} eq "hp3400cl"){$otherMode="";}
                &vlan::portInitialConfiguration($options{"z"},$switchSession[$indiceSwitch],$switchConfig[$indiceSwitch],$otherMode);
            }
            else{
                die "ERROR : you can't modify this port";
            }

        }
        else{&usage();}

    }
    elsif($_ eq "v"){}
    elsif($_ eq "s"){}
    elsif($_ eq "P"){}
    elsif($_ eq "F"){}
    else{
        &usage();
    }

}

sub is_computer_name {
    my $name = shift;
    return ( $name  !~ /^\d+$/  and $name !~ /^\d+\/0\/\d+$/ and  $name !~ /^\d+\/\d+$/);
}

##########################################################################################
# Search the id of the switch in the switch table
# arg : String -> the name of the switch
# ret : the number or -1
# rmq :
##########################################################################################
sub getSwitchIdByName(){
    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="getSwitchIdByName";
    &const::verbose();

    my $indice=-1;

#Check arguement
    my ($name)=@_;
    if(not defined $name){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }

    foreach my $i (0..$#{$switch}) {
        &const::verbose("name : ".$switch->[$i]{"Name"});
        if ($switch->[$i]{"Name"} eq  $name) {
            &const::verbose("found switch $name !");
            $indice = $i;
            last;
        }
    }

    $const::FUNC_NAME=$OLD_FUNC_NAME;

    return $indice;

}

##########################################################################################
# Print the ports affected to a vlan 
# arg : String -> the vlan name
#       Session -> a switch session
#    Config -> a switch configuration
#    String -> the switch name
# ret : 
# rmq : 
##########################################################################################
sub printPortsAffectedToVlan(){


    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="printPortsAffectedToVlan";
    &const::verbose();

#Check arguement
    my ($vlanName,$switchSession,$switchConfig,$switchName)=@_;
    if(not defined $vlanName or not defined $switchSession or not defined $switchConfig or not defined $switchName){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }

    my $ret = $switchConfig->getPortsAffectedToVlan($vlanName,$switchSession);
    #Print the information
    foreach my $type ("TAGGED", "UNTAGGED") {
        print "$type\n";
        my $first=1;
        if(defined  @{$ret->{$type}}){
            foreach my $port (@{ $ret->{$type} }){
                print "," unless $first;
                $first = 0;
                my $name = KaVLAN::Config::getPortName($port,$switchName,$site->{"Name"});
                $name = $port unless $name;
                print $name;
            }
        }
        print "\n";
    }
    $const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Print port information 
# arg : Integer -> the port number
#    Session -> the switch session
#    Config -> a switch configuration
# ret : 
# rmq :
##########################################################################################
sub printPortInformation(){

    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="printPortInformation";
    &const::verbose();

    my @ret;
    my $val;

#Check arguement
    my ($port,$switchSession,$switchConfig)=@_;
    if(not defined $port or not defined $switchSession or not defined $switchConfig){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }

#Get information of the port
    my @info=$switchConfig->getPortInformation($port,$switchSession);

    print "TAGGED :\n";
    foreach my $i (1..$#info){
        print $info[$i];
        print "," unless $i == $#info;
    }
    print "\n";
    print "UNTAGGED :\n";
    if(defined $info[0]){
        print $info[0];
    }
    print "\n";


    $const::FUNC_NAME=$OLD_FUNC_NAME;

}




##########################################################################################
# Know if we can modify this port 
# arg : Integer -> the port
#    Integer -> the switch indice
#       Switch -> a switch tab of reference
# ret : 0 if it's ok OR -1 
# rmq :
##########################################################################################
sub canModifyPort(){


    my $OLD_FUNC_NAME=$const::FUNC_NAME;
    $const::FUNC_NAME="canModifyPort";
    &const::verbose();

#Check arguement
    my ($port,$indice,$switch)=@_;
    if(not defined $port or not defined $indice or not defined $switch){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }

    my $portAllowed=$switch->[$indice]{"Ports"};

    if($portAllowed eq "all"){
        return 0;
    }


    my @portArray = split(/,/,$portAllowed);

    my $trouve=-1;
    my $i;

    for($i=0;$i < ($#portArray+1) && $trouve==-1;$i++){
#If the string matches the format xx-yy that means that it is a range of port
        if($portArray[$i] =~ /-/){
            my $min=$portArray[$i];
            $min =~ s/\-\d+//;
            my $max=$portArray[$i];
            $max =~ s/\d+\-//;
            if($port>=$min && $port<=$max){
                $trouve=0;
            }
        }
        else{
            if($port==$portArray[$i]){
                $trouve=0;
            }
        }

    }

    $const::FUNC_NAME=$OLD_FUNC_NAME;

    return $trouve;


}

##########################################################################################
# Usage
# arg :
# ret : 
# rmq :
##########################################################################################
sub usage(){
print "Version $const::VERSION
USAGE : $0 [options]
==>Options<==
VLAN CONFIGURATION  
=>Those options can be used with the -s 
 -a|--add <vlanName> Add a vlan
 -d|--del <vlanName> Delete a vlan
 -l|--list or -l <vlanName> List informations of vlan 
 -i|--information <vlanName> Ports contained in the vlan
PORT CONFIGURATION (use \"DEFAULT\" as <vlanName> to use the default vlan) 
=>Those options can to be used with the -s or the default switch will be choosen
(The port argument can be a computer name)
 -p|--port <port> Get the vlan on which the port is affected 
 -t|--tag <port> <vlanName> Put a machine in a vlan in Tag mode 
 -u|--untag <port> <vlanName> Put a machine in a vlan in Untag mode
 -r|--remove <port> <vlanName> Remove a machine of a vlan
 -z|--zero <port> Set initial configuration of a port
GLOBAL OPTIONS
 -s|--switch <switchName> Do modification on this switch 
 -F|--config <configFile> Specify the configuration file of kavlan
 -P|--path <path> Where to find corresponding table for the site
 -h|--help Ask for help
 -v|--verbose Active verbose mode\n";
}



