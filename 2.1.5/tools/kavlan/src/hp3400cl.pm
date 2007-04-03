#!/usr/bin/perl -w

##########################################################################################
# Specific file for the hp3400cl 
# author       : Jérémie TISSERAND
# date         : 29/08/2006
# note         : 
##########################################################################################
# version      :
# modified     : 
# author       :
# modification :
##########################################################################################



package hp3400cl; 

#For a better syntaxe
use strict;
#Include SNMP functions
use SNMP;

use const;



my $HP3400CL_VLAN_NAME_FOR_MODIF=".1.3.6.1.2.1.17.7.1.4.3.1.1";
my $HP3400CL_VLAN_NAME=".1.3.6.1.2.1.31.1.1.1.1";
my $HP3400CL_TAG=".1.3.6.1.2.1.16.22.1.1.1.1.4.1.3.6.1.2.1.16.22.1.4.1";
my $HP3400CL_IP=".1.3.6.1.4.1.11.2.14.11.1.4.8.1.1.1";
my $HP3400CL_MASK=".1.3.6.1.4.1.11.2.14.11.1.4.8.1.1.2";
my $HP3400CL_LIST_TAG=".1.3.6.1.2.1.17.7.1.4.3.1.2";
my $HP3400CL_LIST_UNTAG=".1.3.6.1.2.1.17.7.1.4.3.1.4";
my $HP3400CL_NB_PORT=49;
my $HP3400CL_AFFECTED_VALUE="1";
my $HP3400CL_REMOVE_VALUE="0";


##########################################################################################
# Constructor of the object 
# arg : 
# ret : 
# rmq :
##########################################################################################
sub new(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="new";
	&const::verbose();

	my $self ={};
	bless($self);
	
	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return $self;
}

##########################################################################################
# Get the vlan number
# arg : String -> the name of the vlan
#       Session -> the session on which we want to get the number of the vlan
# ret : Integer[] -> the numbers of vlan matches the string passed in argument 
# rmq :
##########################################################################################
sub getVlanNumber(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="getVlanNumber";
	&const::verbose();

	my @res;

	my $self = shift;
#Check arguments
	my ($vlanName,$session)=@_;
	if(not defined $vlanName or not defined $session){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retrieve the number and the name of each vlan
	my $var = new SNMP::VarList([$HP3400CL_VLAN_NAME]);
	my @resp = $session->bulkwalk(0,$const::IEEE_MAX_VLAN,$var);

#Loop until we have a name which correspond to $vlanName
	my $i;
	for($i=0; ( $i<$const::IEEE_MAX_VLAN ) && ( $i<($#{ @{ $resp[0] } } +1) ) ;$i++){
#		&const::verbose("Seeing ", ${ @{ $resp[0] } }[$i]->val);
		if( ${ @{ $resp[0] } }[$i]->val =~ /$vlanName/){
			&const::verbose("Adding vlan ", ${ @{ $resp[0] } }[$i]->val ," because he matches the given name");
#Getting the end of the oid as vlanNumber
                        my $number = ${@{$resp[0]}}[$i]->iid;
                        if(not defined $number or $number eq ""){
                                $number = ${@{${@{$resp[0]}}[$i]}}[0];
                        }

			$number =~ s/\w+\.//g;
			$number =~ s/\D+//g;

			push @res, $number;
		}
		
	}

#Return the value which correspond to the vlan name
	&const::verbose("Vlan's availables ",@res);
	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return @res;

}

##########################################################################################
# Get the vlan name 
# arg : Integer -> the vlan number
#       Session -> the session on which we want to get the number of the vlan
# ret : String -> the name of the vlan
# rmq :
##########################################################################################
sub getVlanName(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="getVlanName";
	&const::verbose();

	my $self = shift;
#Check arguments
	my ($vlanNumber,$session)=@_;
	if(not defined $vlanNumber or not defined $session){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retrieve the number and the name of each vlan
	my $var = new SNMP::Varbind([$HP3400CL_VLAN_NAME,$vlanNumber]);
	my $resp = $session->get($var);

	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return $resp;

}
##########################################################################################
# Modify a vlan name 
# arg : String -> the old name of the vlan
# 	String -> the new name of the vlan 
#       Session -> the session on which we want to change the vlan name
# ret : 
# rmq : The number have to be retrieved by using the 'getVlanNumber' function
##########################################################################################
sub modifyVlanName(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="modifyVlanName";
	&const::verbose();

	my $self = shift;
#Check arguement
	my ($oldVlanName,$newVlanName,$session)=@_;
	if(not defined $oldVlanName or not defined $newVlanName or not defined $session){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive the vlan number
	my @vlanNumber = $self->getVlanNumber($oldVlanName,$session);
	if($#vlanNumber==-1){
		die "ERROR : Can't modify the vlan name because there is vlan available";
	}
	
	$vlanNumber[0] = $self->getTagConfiguration($vlanNumber[0],$session);

#Create the snmp variable to apply changes in the vlan $vlanNumber
	my $var=new SNMP::Varbind([$HP3400CL_VLAN_NAME_FOR_MODIF,$vlanNumber[0],$newVlanName,"OCTETSTR"]);

#Send the snmp information
	&const::verbose("Applying modification");

	$session->set($var) or die "ERROR : Can't modify vlan (there is probably another vlan with the same name)\n";

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}
##########################################################################################
# Get the IP Configuration of a vlan 
# arg : Integer -> the number of the vlan on the routeur session 
#       Session -> a session on which we can get the IP address
# ret : String -> the IP configuration 'IP/MASK'
# rmq :
##########################################################################################
sub getIPConfiguration(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="getIPConfiguration";
	&const::verbose();

	my $self = shift;
#Check arguement
	my ($vlanNumber,$session)=@_;
	if(not defined $vlanNumber or not defined $session){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive the informations about ip configuration	
	&const::verbose("Retreive ip configuration of the vlan");
	my $ip=new SNMP::Varbind([$HP3400CL_IP,$vlanNumber,"","NETADDR"]);
	my $mask=new SNMP::Varbind([$HP3400CL_MASK,$vlanNumber,"","NETADDR"]);
	$session->get($ip) or die "ERROR : Can't retreive information about ip adress of the vlan";
	$session->get($mask) or die "ERROR : Can't retreive information about mask format of the vlan";
	
	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return $ip->val."/".$mask->val;

}


##########################################################################################
# Get the tag of a vlan 
# arg : Integer -> the number of the vlan on the routeur session 
#       Session -> a session on which we can get the tag information
# ret : String -> the tag 
# rmq :
##########################################################################################
sub getTagConfiguration(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="getTagConfiguration";
	&const::verbose();

	my $self = shift;
#Check arguement
	my ($vlanNumber,$session)=@_;
	if(not defined $vlanNumber or not defined $session){
		die "ERROR : Not enough argument for $const::FUNC_NAME=";
	}

#Retreive the informations about ip configuration	
	&const::verbose("Retreive tag configuration of the vlan for ".$vlanNumber);
	my $res = 0;

#Retrieve the number and the name of each vlan
	my $var = new SNMP::VarList([$HP3400CL_TAG]);
	my @resp = $session->bulkwalk(0,$const::IEEE_MAX_VLAN,$var);

#Loop until we have a name which correspond to $vlanName
	my $i;
	for($i=0; ( $i<$const::IEEE_MAX_VLAN ) && ( $i<($#{ @{ $resp[0] } } +1) ) && ($res == 0) ;$i++){
		
		my $tag;
		my $number;
#If the it is the normal mode, the tag is the value and number is in the oid
		$number = ${ @{ $resp[0] } }[$i]->val;
		$tag = ${@{$resp[0]}}[$i]->iid;
		if(not defined $tag or $tag eq ""){
			$tag = ${@{${@{$resp[0]}}[$i]}}[0];
		}
		$tag =~ s/\w+\.//g;
		$tag =~ s/\D+//g;
		if($number == $vlanNumber){
			$res = $tag;
		}
	
	}
	
	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return $res;
}



##########################################################################################
# List vlan that matches a name 
# arg : String -> the vlan name
#       Session -> the routeur session
# ret : 
# rmq :
##########################################################################################
sub listVlanOnRouteur(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="listVlanOnRouteur";
	&const::verbose();

#Check arguement
	my $self = shift;
	my ($vlanName,$routeurSession)=@_;
	if(not defined $vlanName or not defined $routeurSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retrieve the number and the name of each vlan
	my $var = new SNMP::VarList([$HP3400CL_VLAN_NAME]);
	my @resp = $routeurSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$var);

#Loop until we have a name which correspond to $vlanName
	my $i;
	for($i=0; ( $i<$const::IEEE_MAX_VLAN ) && ( $i<($#{ @{ $resp[0] } } +1) ) ;$i++){
		if(${ @{ $resp[0] } }[$i]->val =~ /$const::MODIFY_NAME_KAVLAN$vlanName/){
			&const::verbose("Vlan founded:",${ @{ $resp[0] } }[$i]->val);

			print "-------------------------------\n";
			my $val = ${ @{ $resp[0] } }[$i]->val;
			$val =~ s/$const::MODIFY_NAME_KAVLAN//;
			print "VLAN NAME : $val\n";
#Get the vlan number in order to retreive informations
			my @vlanNumber = $self->getVlanNumber($val,$routeurSession);
			my $ip =  $self->getIPConfiguration($vlanNumber[0],$routeurSession);
			print "ATTRIBUTED IP : $ip\n";
			my $tag = $self->getTagConfiguration($vlanNumber[0],$routeurSession);
			print "VLAN TAG : $tag\n";

		}
		if($vlanName =~ /$const::DEFAULT_NAME/ && ${ @{ $resp[0] } }[$i]->val =~ /$const::VLAN_DEFAULT_NAME/){
			print "-------------------------------\n";
			my $val = $const::DEFAULT_NAME;
			print "VLAN NAME : $val\n";
			$val =  $const::VLAN_DEFAULT_NAME;
			my @vlanNumber = $self->getVlanNumber($val,$routeurSession);
			my $ip =  $self->getIPConfiguration($vlanNumber[0],$routeurSession);
			print "ATTRIBUTED IP : $ip\n";
			my $tag = $self->getTagConfiguration($vlanNumber[0],$routeurSession);
			print "VLAN TAG : $tag\n";
		}
	
	}

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}


##########################################################################################
# List vlan that matches a name 
# arg : String -> the vlan name
#	Session -> the switch session
# ret : 
# rmq :
##########################################################################################
sub listVlanOnSwitch(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="listVlanOnSwitch";
	&const::verbose();

#Check arguement
	my $self = shift;
	my ($vlanName,$switchSession,$switchConfig)=@_;
	if(not defined $vlanName or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retrieve the number and the name of each vlan
	my $var = new SNMP::VarList([$HP3400CL_VLAN_NAME]);
	my @resp = $switchSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$var);

#Loop until we have a name which correspond to $vlanName
	my $i;
	for($i=0; ( $i<$const::IEEE_MAX_VLAN ) && ( $i<($#{ @{ $resp[0] } } +1) ) ;$i++){
		if(${ @{ $resp[0] } }[$i]->val =~ /$const::MODIFY_NAME_KAVLAN$vlanName/){
			&const::verbose("Vlan founded:",${ @{ $resp[0] } }[$i]->val);

			print "-------------------------------\n";
			my $val = ${ @{ $resp[0] } }[$i]->val;
			$val =~ s/$const::MODIFY_NAME_KAVLAN//;
			print "VLAN NAME : $val\n";

		}
#The case of the default vlan
		if($vlanName =~ /$const::DEFAULT_NAME/ && ${ @{ $resp[0] } }[$i]->val =~ /$const::VLAN_DEFAULT_NAME/){
			print "-------------------------------\n";
			my $val = $const::DEFAULT_NAME;
			print "VLAN NAME : $val\n";
		}
	}

	$const::FUNC_NAME=$OLD_FUNC_NAME;

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

#Check arguement
	my $self=shift;
	my ($vlanName,$switchSession)=@_;
	if(not defined $vlanName or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}


#Get port informations
	&const::verbose("Getting ports affected");	

#Retreive the vlan number
        my @vlanNumber;
        if($vlanName eq $const::DEFAULT_NAME){
                @vlanNumber= $self->getVlanNumber($const::VLAN_DEFAULT_NAME,$switchSession);
        }
        else{
                @vlanNumber= $self->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);
        }
	if($#vlanNumber == -1){
		die "ERROR : There is no vlan under this name";
	}

	$vlanNumber[0] = $self->getTagConfiguration($vlanNumber[0],$switchSession);

#Get the information
	my $untag =new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0]]);
	my $tag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0]]);

        $switchSession->get($untag);
        $switchSession->get($tag);
	
	my $untagInfo = unpack("B*",$untag->val);
	my $tagInfo = unpack("B*",$tag->val);
	

#Look if there are any ports affected in the tag or untag mode
        my $i;
        for($i=0;$i<($HP3400CL_NB_PORT+1);$i++){
#Retreive the oid sometimes, the oid is not in the good field and is in the name of the object that's why we try two way to get this number
       		my $valUntag =  substr($untagInfo,$i,1); 
       		my $valTag =  substr($tagInfo,$i,1); 

		if($valTag eq $HP3400CL_AFFECTED_VALUE && $valUntag eq $HP3400CL_AFFECTED_VALUE){
			push @{$res{"UNTAGGED"}}, ($i+1);
		}
		elsif($valTag eq $HP3400CL_AFFECTED_VALUE){
			push @{$res{"TAGGED"}}, ($i+1);
		}
		
      }

	 $const::FUNC_NAME=$OLD_FUNC_NAME;

	return \%res;
}


##########################################################################################
# Get port information 
# arg : Integer -> the port number
#	Session -> the switch session
# ret : a tab containing on the first element the untag vlan and on the others the tagged vlan
#       on which the port is affected
# rmq :
##########################################################################################
sub getPortInformation(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="getPortInformation";
	&const::verbose();

	my @ret;
	my $val;

#Check arguement
	my $self = shift;
	my ($port,$switchSession)=@_;
	if(not defined $port or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retrieve the number and the name of each vlan
	my $var = new SNMP::VarList([$HP3400CL_VLAN_NAME]);
	my @resp = $switchSession->bulkwalk(0,$const::IEEE_MAX_VLAN,$var);

	my $indiceTagPort = 1;
#Loop until we have a name which correspond to $vlanName
	my $i;
	for($i=0; ( $i<$const::IEEE_MAX_VLAN ) && ( $i<($#{ @{ $resp[0] } } +1) ) ;$i++){

		my $name = ${ @{ $resp[0] } }[$i]->val;

		if($name =~ /$const::MODIFY_NAME_KAVLAN/ || $name =~ /$const::VLAN_DEFAULT_NAME/){	
#Getting the right name of the vlan (without the prefix MODIFY_NAME_VLAN)	
			if($name =~ /$const::MODIFY_NAME_KAVLAN/){ 
				&const::verbose("Vlan founded:",$name);
				$val = $name;
				$val =~ s/$const::MODIFY_NAME_KAVLAN//;
	
			}
			if($name =~ /$const::VLAN_DEFAULT_NAME/){
				&const::verbose("Vlan founded:",$name);
				$val = $const::DEFAULT_NAME;
			}
		
			my $tmp = $self->getPortsAffectedToVlan($val,$switchSession);
			my %res = %{$tmp};

			my $j;
#Add informations about this vlan if we find the port in the vlan
			if(defined  @{$res{"TAGGED"}}){
				for($j=0;$j<$#{ @{ $res{"TAGGED"} } }+1;$j++){
					if( ${@{$res{"TAGGED"}}}[$j] == $port){
						$ret[$indiceTagPort] = $val;
						$indiceTagPort++;
					}
				}
			}
			if(defined  @{$res{"UNTAGGED"}}){
				for($j=0;$j<$#{ @{ $res{"UNTAGGED"} } }+1;$j++){
					
					if(${@{$res{"UNTAGGED"}}}[$j] == $port){
						$ret[0] = $val;
					}
				}
			}

		}

	}

	

	$const::FUNC_NAME=$OLD_FUNC_NAME;

	return @ret;

}





##########################################################################################
# Set a port as tag 
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
# ret : 
# rmq :
##########################################################################################
sub setTag(){
	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="setTag";
	&const::verbose();

#Check arguement
	my $self = shift;
	my ($vlanName,$port,$switchSession)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive the vlan number of $vlanName
	&const::verbose("Verifying that the vlan is available");
	my @vlanNumber;
	if($vlanName eq $const::DEFAULT_NAME){
		@vlanNumber= $self->getVlanNumber($const::VLAN_DEFAULT_NAME,$switchSession);
	}
	else{
		@vlanNumber= $self->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);
	}
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan available";
	}

	$vlanNumber[0] = $self->getTagConfiguration($vlanNumber[0],$switchSession);
#Change the port information

	&const::verbose("Put the port in tagged mode ",$port," to the vlan ",$vlanNumber[0]);	
	my $tag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0]]);
	my $untag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0]]);

	$switchSession->get($tag);
	$switchSession->get($untag);	

	my $depTag = $tag->val;
	my $depUntag = $untag->val;

#	print unpack("H*",$untag->val);
#	print "\n";
#	print unpack("H*",$tag->val);
#	print "\n";

	my $untagInfo = unpack("B*",$untag->val);
	my $tagInfo = unpack("B*",$tag->val);
	
	substr($untagInfo,($port-1),1)="0";
	substr($tagInfo,($port-1),1)="1";

#	print $untagInfo;
#	print "\n";
#	print $tagInfo;
#	print "\n";

	$untagInfo = pack("B*",$untagInfo);
	$tagInfo = pack("B*",$tagInfo);	

#	print unpack("H*",$untagInfo);
#	print "\n";
#	print unpack("H*",$tagInfo);
#	print "\n";

#	if($depTag ne $tagInfo && $depUntag eq $untagInfo){print "OK\n";}
#	else{print "KO\n";}

        my $newTag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0],$tagInfo,"OCTETSTR"]);
	my $newUntag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0],$untagInfo,"OCTETSTR"]);

	$switchSession->set($newTag) or die "ERROR : Can't affect the port to the vlan";
	$switchSession->set($newUntag) or die "ERROR : Can't affect the port to the vlan";

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Set a port as untag 
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
# ret : 
# rmq :
##########################################################################################
sub setUntag(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="setUntag";
	&const::verbose();

#Check arguement
	my $self = shift;
	my ($vlanName,$port,$switchSession)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive the vlan number of $vlanName
	&const::verbose("Verifying that the vlan is available");
	my @vlanNumber;
	if($vlanName eq $const::DEFAULT_NAME){
		@vlanNumber= $self->getVlanNumber($const::VLAN_DEFAULT_NAME,$switchSession);
	}
	else{
		@vlanNumber= $self->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);
	}
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan available";
	}

	$vlanNumber[0] = $self->getTagConfiguration($vlanNumber[0],$switchSession);
#Change the port information

	&const::verbose("Put the port in untag mode ",$port," to the vlan ",$vlanNumber[0]);	
	my $tag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0]]);
	my $untag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0]]);

	$switchSession->get($tag);
	$switchSession->get($untag);	

	my $depTag = $tag->val;
	my $depUntag = $untag->val;

	my $untagInfo = unpack("B*",$untag->val);
	my $tagInfo = unpack("B*",$tag->val);
	
	substr($untagInfo,($port-1),1)="1";
	substr($tagInfo,($port-1),1)="1";

	$untagInfo = pack("B*",$untagInfo);
	$tagInfo = pack("B*",$tagInfo);	

        my $newTag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0],$tagInfo,"OCTETSTR"]);
	my $newUntag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0],$untagInfo,"OCTETSTR"]);

	$switchSession->set($newUntag) or die "ERROR : Can't affect the port to the vlan";
	$switchSession->set($newTag) or die "ERROR : Can't affect the port to the vlan";

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}

##########################################################################################
# Set a port as remove 
# arg : String -> the vlan name
#       Integer -> the port
#	Session -> a switch session
# ret : 
# rmq :
##########################################################################################
sub setRemove(){

	my $OLD_FUNC_NAME=$const::FUNC_NAME;
	$const::FUNC_NAME="removePort";
	&const::verbose();


#Check arguement
	my $self = shift;
	my ($vlanName,$port,$switchSession)=@_;
	if(not defined $vlanName or not defined $port or not defined $switchSession){
		die "ERROR : Not enough argument for $const::FUNC_NAME";
	}

#Retreive the vlan number of $vlanName
	&const::verbose("Verifying that the vlan is available");
	my @vlanNumber;
	if($vlanName eq $const::DEFAULT_NAME){
		@vlanNumber= $self->getVlanNumber($const::VLAN_DEFAULT_NAME,$switchSession);
	}
	else{
		@vlanNumber= $self->getVlanNumber($const::MODIFY_NAME_KAVLAN.$vlanName,$switchSession);
	}
	if($#vlanNumber==-1){
		die "ERROR : There is no vlan available";
	}

	$vlanNumber[0] = $self->getTagConfiguration($vlanNumber[0],$switchSession);
#Change the port information

	&const::verbose("Remove the port ",$port," from the vlan ",$vlanNumber[0]);	
	my $tag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0]]);
	my $untag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0]]);

	$switchSession->get($tag);
	$switchSession->get($untag);	

	my $depTag = $tag->val;
	my $depUntag = $untag->val;

	my $untagInfo = unpack("B*",$untag->val);
	my $tagInfo = unpack("B*",$tag->val);
	
	substr($untagInfo,($port-1),1)="0";
	substr($tagInfo,($port-1),1)="0";

	$untagInfo = pack("B*",$untagInfo);
	$tagInfo = pack("B*",$tagInfo);	

        my $newTag = new SNMP::Varbind([$HP3400CL_LIST_TAG,$vlanNumber[0],$tagInfo,"OCTETSTR"]);
	my $newUntag = new SNMP::Varbind([$HP3400CL_LIST_UNTAG,$vlanNumber[0],$untagInfo,"OCTETSTR"]);

	$switchSession->set($newUntag) or die "ERROR : Can't remove the port from the vlan";
	$switchSession->set($newTag) or die "ERROR : Can't remove the port from the vlan";

	$const::FUNC_NAME=$OLD_FUNC_NAME;

}







1;

