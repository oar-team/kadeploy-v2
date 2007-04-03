#!/usr/bin/perl -w

##########################################################################################
# VLAN modification function file 
# author       : Jérémie TISSERAND
# date         : 29/08/2006
# note         : 
##########################################################################################
# version      :
# modified     : 
# author       :
# modification :
##########################################################################################



package vlan;

#For a better syntaxe
use strict;

use SNMP;

use const;

##########################################################################################
# Add a vlan
# arg : String -> the name of the vlan
#       Session -> a routeur session
#	Config -> a routeur configuration
# ret : 
# rmq :
##########################################################################################
sub addVlanOnRouteur(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="addVlanOnRouteur";
	&const::verbose();

#Check arguement
	my ($vlanName,$routeurSession,$routeurConfig)=@_;
	if(not defined $vlanName or not defined $routeurSession or not defined $routeurConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}


	if($vlanName eq $const::DEFAULT_NAME){
		die "ERROR : this name can not be used";
	}

#Retreive the vlan number of $vlanName
	&const::verbose("Verifying that there is enough vlan available");
	my @vlanNumber= $routeurConfig->getVlanNumber($const::DEFAULT_NAME_KAVLAN,$routeurSession);
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan available for modification";
	}

#Change the name of the vlan on the routeur
	&const::verbose("Modifying vlan name on the routeur");
	$routeurConfig->modifyVlanName($const::DEFAULT_NAME_KAVLAN,$const::MODIFY_NAME_KAVLAN.$vlanName,$routeurSession);

#Get the ip and tag information
	print "VLAN NAME : $vlanName\n";
	my $ipInformation = $routeurConfig->getIPConfiguration($vlanNumber[0],$routeurSession);
	print "ATTRIBUTED IP : $ipInformation\n";  
	my $tagInformation = $routeurConfig->getTagConfiguration($vlanNumber[0],$routeurSession);
	print "VLAN TAG : $tagInformation\n";

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Add a vlan
# arg : String -> the name of the vlan
#       Session -> a switch session
#	Config -> a switch configuraiton
# ret : 
# rmq :
##########################################################################################
sub addVlanOnSwitch(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="addVlanOnSwitch";
	&const::verbose();

#Check arguement
	my ($vlanName,$switchSession,$switchConfig)=@_;
	if(not defined $vlanName or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}


	if($vlanName eq $const::DEFAULT_NAME){
		die "ERROR : this name can not be used";
	}

#Retreive the vlan number of $vlanName
	&const::verbose("Verifying that there is enough vlan available");
	my @vlanNumber= $switchConfig->getVlanNumber($const::DEFAULT_NAME_KAVLAN,$switchSession);
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan available for modification";
	}

#Change the name of the vlan on the switch 
	&const::verbose("Modifying vlan name on the switch");
	$switchConfig->modifyVlanName($const::DEFAULT_NAME_KAVLAN,$const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Delete a vlan
# arg : String -> the name of the vlan
#       Session -> a switch session
#	Config -> a routeur configuration
# ret : 
# rmq :
##########################################################################################
sub delVlanOnRouteur(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="delVlanOnRouteur";
	&const::verbose();

	
#Check arguement
	my ($vlanName,$routeurSession,$routeurConfig)=@_;
	if(not defined $vlanName or not defined $routeurSession or not defined $routeurConfig ){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

	if($vlanName eq $const::DEFAULT_NAME){
		die "ERROR : this name can not be used";
	}
#Retreive the vlan number of $vlanName
	&const::verbose("Verifying if this vlan is set");
	my @vlanNumber= $routeurConfig->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$routeurSession);
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan under this name";
	}

#Change the name of the vlan
	&const::verbose("Modifying vlan name on the routeur");
	$routeurConfig->modifyVlanName($const::MODIFY_NAME_KAVLAN.$vlanName,$const::DEFAULT_NAME_KAVLAN.$vlanNumber[0],$routeurSession);

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}
##########################################################################################
# Delete a vlan
# arg : String -> the name of the vlan
#       Session -> a switch session
#	Config -> a switch configuration
#	Var -> specify the cleaner mode to delete vlan
# ret : 
# rmq :
##########################################################################################
sub delVlanOnSwitch(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="delVlanOnSwitch";
	&const::verbose();

	
#Check arguement
	my ($vlanName,$switchSession,$switchConfig,$otherMode)=@_;
	if(not defined $vlanName or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

	if($vlanName eq $const::DEFAULT_NAME){
		die "ERROR : this name can not be used";
	}
#Retreive the vlan number of $vlanName
	&const::verbose("Verifying if this vlan is set");
	my @vlanNumber= $switchConfig->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan under this name";
	}

#Remove all the ports affected to this vlan
	&const::verbose("Removing ports from the vlan");
	my $port = $switchConfig->getPortsAffectedToVlan($vlanName,$switchSession);
	my %res = %{$port};
        
	my $i;
	if(defined  @{$res{"TAGGED"}}){
                for($i=0;$i<$#{ @{ $res{"TAGGED"} } }+1;$i++){
        		&removePort($vlanName,${@{$res{"TAGGED"}}}[$i],$switchSession,$switchConfig,$otherMode);
                }
        }
	if(defined  @{$res{"UNTAGGED"}}){
		for($i=0;$i<$#{ @{ $res{"UNTAGGED"} } }+1;$i++){
			&removePort($vlanName,${@{$res{"UNTAGGED"}}}[$i],$switchSession,$switchConfig,$otherMode);
		}
	}
	

#Change the name of the vlan
	&const::verbose("Modifying vlan name on the switch");
	$switchConfig->modifyVlanName($const::MODIFY_NAME_KAVLAN.$vlanName,$const::DEFAULT_NAME_KAVLAN.$vlanNumber[0],$switchSession);
	
	$const::FUNC_NAME=$OLD_FUNC_NAME;

}


##########################################################################################
# Add a machine in the tagged mode for a vlan 
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
#	Config -> a switch configuration
#	Var -> specify the cleaner mode to delete the port 
# ret : 
# rmq :
##########################################################################################
sub addTaggedPort(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="addTaggedPort";
	&const::verbose();

#Check arguement
	my ($vlanName,$port,$switchSession,$switchConfig,$otherMode)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}
	
#	&removePort($vlanName,$port,$switchSession,$switchConfig,$otherMode);

	$switchConfig->setTag($vlanName,$port,$switchSession);

	$const::FUNC_NAME=$OLD_FUNC_NAME;


}


##########################################################################################
# Add a machine in the untagged mode for a vlan 
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
#	Config -> a switch configuration
#	Var -> specify the cleaner mode to delete the port 
# ret : 
# rmq :
##########################################################################################
sub addUntaggedPort(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="addUntaggedPort";
	&const::verbose();

#Check arguement
	my ($vlanName,$port,$switchSession,$switchConfig,$otherMode)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}


	my @res = $switchConfig->getPortInformation($port,$switchSession);
#If we are trying to put the port in the vlan but he is already here
	if(defined $res[0] && $res[0] eq $vlanName){
		$const::FUNC_NAME=$OLD_FUNC_NAME;
		return ;
	}



	if(defined $otherMode){
		&const::verbose("clear mode activated");
#If we are with hp switch, we can't affect as untag before removing the older untag vlan and we can't let a port unaffect, that's why we are using the tag mode	
		if(defined $res[0]){
			$switchConfig->setTag($res[0],$port,$switchSession);
		}
		$switchConfig->setTag($vlanName,$port,$switchSession);
		$switchConfig->setUntag($vlanName,$port,$switchSession);
		if(defined $res[0]){
			$switchConfig->setRemove($res[0],$port,$switchSession);
		}
	}
	else{
#If the port is tag, we remove it
		my $trouve=0;
		for(my $i=1; $i<($#res+1) && $trouve==0; $i++){
			if(defined $res[$i] && $res[$i] eq $vlanName){$trouve=1;}
		}
		if($trouve==1){ $switchConfig->setRemove($vlanName,$port,$switchSession);}
		$switchConfig->setUntag($vlanName,$port,$switchSession);
	}	


	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Remove a port from a vlan
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
#	Config -> a switch configuration
#	Var -> specify the cleaner mode to delete the port 
# ret : 
# rmq :
##########################################################################################
sub removePort(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="removePort";
	&const::verbose();

#Check arguement
	my ($vlanName,$port,$switchSession,$switchConfig,$otherMode)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive port informations
	
	my $res=$switchConfig->getPortsAffectedToVlan($vlanName,$switchSession);
	my %infoPort = %{$res};
	
	my $i;
#If we are in the untag mode, we remove the port from the vlan and affect him to the default vlan
	if(defined  @{$infoPort{"UNTAGGED"}}){
		for($i=0;$i<$#{ @{ $infoPort{"UNTAGGED"} } }+1;$i++){
			if(${@{$infoPort{"UNTAGGED"}}}[$i] == $port){
				if(defined $otherMode){
#We are doing this because hp switch doesn't allow to remove a port from a vlan before affecting him to another
					&const::verbose("clear mode activated");
                                        $switchConfig->setTag($vlanName,$port,$switchSession);
					$switchConfig->setTag($const::DEFAULT_NAME,$port,$switchSession);
                                        $switchConfig->setUntag($const::DEFAULT_NAME,$port,$switchSession);
					$switchConfig->setRemove($vlanName,$port,$switchSession);
                                }
                                else{
					$switchConfig->setRemove($vlanName,$port,$switchSession);
                                        $switchConfig->setUntag($const::DEFAULT_NAME,$port,$switchSession);
                                }

			}
		}
	}
#If we are in tag mode, we remove the port
	if(defined @{$infoPort{"TAGGED"}}){
                for($i=0;$i<$#{ @{ $infoPort{"TAGGED"} } }+1;$i++){
                        if(${@{$infoPort{"TAGGED"}}}[$i] == $port){
				my @vlanOfPort = $switchConfig->getPortInformation($port,$switchSession);
#We are putting the port in the default vlan in untag mode before removing it if it was the last vlan in which it belong
				if($#vlanOfPort == 1 && not defined $vlanOfPort[0]){
					&const::verbose("We are putting the $port in the default vlan");
					if(defined $otherMode){
						$switchConfig->setTag($const::DEFAULT_NAME,$port,$switchSession);
	                                        $switchConfig->setUntag($const::DEFAULT_NAME,$port,$switchSession);
					}
					else{
						$switchConfig->setUntag($const::DEFAULT_NAME,$port,$switchSession);
					}
				}
				$switchConfig->setRemove($vlanName,$port,$switchSession);
			}
		}
	}

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Set the initial configuration for a port 
# arg : Integer -> the port
#       Session -> a switch session
#	Config -> a switch configuration
# ret : 
# rmq :
##########################################################################################
sub portInitialConfiguration(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="portInitialConfiguration";
	&const::verbose();

#Check arguement
	my ($port,$switchSession,$switchConfig,$otherMode)=@_;
	if(not defined $port or not defined $switchSession or not defined $switchConfig){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}


#Retreive the vlan number of default VLAN
	&const::verbose("Verifying that the vlan is available");
	my @vlanNumberDefault= $switchConfig->getVlanNumber($const::VLAN_DEFAULT_NAME,$switchSession);
	if($#vlanNumberDefault==-1){
		die "ERROR : Vlan default name is not correctly set on the configuration file";
	}

#Change the port information

	&const::verbose("Put the port ",$port," to the default vlan ");	
	&addUntaggedPort($const::DEFAULT_NAME,$port,$switchSession,$switchConfig,$otherMode);

#Remove the port of all vlan for the tag mode
	&const::verbose("Retreiving all the vlan created");
	my @vlanNumber=$switchConfig->getVlanNumber($const::MODIFY_NAME_KAVLAN,$switchSession);
	my $i;
	for($i=0;$i<($#vlanNumber+1);$i++){
		my $vlanName = $switchConfig->getVlanName($vlanNumber[$i],$switchSession);
		$vlanName =~ s/$const::MODIFY_NAME_KAVLAN//;
		
		&removePort($vlanName,$port,$switchSession,$switchConfig,$otherMode);
	}

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}


1;

