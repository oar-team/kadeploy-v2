#!/usr/bin/perl -w

##########################################################################################
# Config Class
# author       : Nicolas Niclausse
# date         : 02/10/2008
##########################################################################################

package KaVLAN::Config;

@EXPORT = qw(parseConfigurationFile getPortNumber getPortName);

use warnings;
use strict;

use const;


##########################################################################################
# Parse the configuration file
# arg :
# ret : two hash tables references : 1-site, 2-routeur
#       an array reference containing hash tables : 3-switch
# rmq :
##########################################################################################
sub parseConfigurationFile{
    my $key;
    my $value;
    my %site;
    my @switch;
    my %routeur;
    my $typeLine="";

    my $nbSwitch = -1;

    open(CONF,"<",$const::CONFIGURATION_FILE) or die "ERROR : Can't open configuration file";

    while(<CONF>){
        #Verify if a line is not a comment (begin with #) and is a good line (contain a '=' or a '@')
        if( $_ !~ /#\w*/ and ($_ =~ m/=/ or $_ =~ m/@/) ){
#Verify if it is a global information line
            if($_ =~ /@\w*/){
#remove @
                $_ =~ s/@//;
#Inform that the following lines will be for the block name we have just read
                $typeLine=$_;

                if($typeLine =~m/Switch/){
                    $nbSwitch++;
                }
            } else {
#Remove spaces from the line
                $_ =~ s/ //g;
#Remove the \n and the \t
                $_ =~ s/\n//;
                $_ =~ s/\t//g;
#Split the informations around the '='
                ($key,$value) = split(/=/,$_);
#Do the association between the line and the upper block which is the information for
                if($typeLine =~ m/Switch/){
                    $switch[$nbSwitch]{$key}=$value;
                } elsif($typeLine =~ m/Routeur/) {
                    $routeur{$key} = $value;
                } elsif($typeLine =~ m/Site/) {
                    $site{$key} = $value;
                } else {
                    print "Configuration line not associated with an upper block (Switch, Routeur or Information):\n";
                }
            }
        }
    }
    close(CONF);

    if(not defined $routeur{"Name"} or not defined $routeur{"IP"} or not defined $routeur{"Type"}){
        die "ERROR : You have to enter value for the routeur in the configuration file : Name, IP, Type";
    }

    foreach my $i (0..$nbSwitch) {
        if(not defined $switch[$i]{"Name"} or not defined $switch[$i]{"IP"} or not defined $switch[$i]{"Type"} or not defined $switch[$i]{"Ports"}){
            die "ERROR : You have to enter value for the switch in the configuration file : Name, IP, Type";
        }
    }
    if(not defined $site{"VlanDefaultName"} or not defined $site{"Name"}){
        die "ERROR : You have to enter value for the site in the configuration file : VlanDefaultName, Name. You can also enter a value for the SNMPCommunity";
    }

#Return references of the three hash
    return \%site,\%routeur,\@switch;
}


##########################################################################################
# Get the port number via a name
# arg : String -> the name of the computer
#    String -> the site name
# ret : two variable : the number or -1 / the switch name
# rmq :
##########################################################################################
sub getPortNumber {
    my ($portName,$name)=@_;
    # Check arguement
    if(not defined $portName or not defined $name){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }
    my $trouve = 0;
    my $lineName;
    my $linePort;
    my $lineSwitch;
    my $line;

    if(open(CONF,"<".$const::PATH_TABLE_CORES."/".$name.".conf")){
        while( (defined ($line = <CONF>)) && ($trouve==0)){
            $line =~ s/\n//;
            ($lineName,$linePort,$lineSwitch) = split(/ /,$line);
            if(defined $lineName and $lineName eq $portName){
                $trouve = 1;
                &const::verbose("Port founded in the configuration file");
            }

        }
    }
    close(CONF);
    return (-1,-1) unless ($trouve);
    return ($linePort,$lineSwitch);
}


##########################################################################################
# Get the port name via a number
# arg : Integer -> the port number
#     String -> the switch name
#     String -> the site name
# ret : the port name or ""
# rmq :
##########################################################################################
sub getPortName{
    my ($portNumber,$switchName,$siteName)=@_;
    my $trouve = 0;
    my $lineName;
    my $linePort;
    my $lineSwitch;
    my $line;
#Check arguement
    if(not defined $portNumber or not defined $switchName or not defined $siteName){
        die "ERROR : Not enough argument for $const::FUNC_NAME";
    }


    if(open(CONF,"<",$const::PATH_TABLE_CORES.$siteName.".conf")){
        while( (defined ($line = <CONF>)) && ($trouve==0)){
            $line =~ s/\n//;
            ($lineName,$linePort,$lineSwitch) = split(/ /,$line);
            if(defined $lineSwitch and defined $linePort and $lineSwitch eq $switchName and $linePort eq $portNumber){
                $trouve = 1;
                &const::verbose("Port founded in the configuration file");
            }

        }
    }
    close(CONF);
    return "" unless ($trouve);
    return $lineName;
}

1;
