# This is the iolib, which manages the layer between the modules and the
# database. This is the only base-dependent layer.
# When adding a new function, we recommend to :
# - give the name of the function
# - start with a short description of the function
# - list the parameters it expects
# - list the return values
# - list the side effects

package deploy_iolib;

use DBI;
use lib::conflib qw(init_conf get_conf is_conf);
use Time::Local;
use strict;

##############
# PROTOTYPES #
##############

# database connectors #
sub connect();
sub disconnect($);

# deployment action tools #
sub prepare_deployment($);
sub run_deployment($$);
sub end_deployment($$);
sub cancel_deployment($$);

# deployment partition selection tools #
sub get_partition_status($$@);
sub get_deploy_id($$$$);
sub search_deployed_env($$$);
sub autochoose_partition($%);

# database accessors #
sub env_name_ver_to_id($$$);
sub env_name_to_last_ver_id($$);
sub env_name_to_versions($$);
sub env_id_to_name($$);
sub env_id_to_size($$);
sub env_id_to_version($$);
sub env_name_to_filesystem($$);
sub env_name_to_kernel($$);
sub env_name_to_filebase($$);
sub env_name_to_filesite($$);
sub env_name_to_size($$);
sub env_undefined_to_id($);
sub disk_id_to_dev($$);
sub disk_dev_to_id($$);
sub part_nb_to_id($$$);
sub part_id_to_nb($$);
sub part_id_to_size($$);
sub node_name_to_id($$);
sub node_id_to_name($$);
sub node_name_to_ip($$);
sub node_ip_to_name($$);
sub node_name_to_name($$);

sub deploy_id_to_env_info($$);
sub deploy_id_to_node_info($$);

# kaaddnode/kadelnode tools #
sub add_node($$);
sub add_deploy($$$$);
sub add_disk($$);
sub add_partition($$$);
sub add_environment($$$$$$$$$$$$$$);
sub del_node($$);

# deployment tools #
sub report_state($$$$$);
sub is_node_free($$$);
sub add_node_to_deployment($$$$$$);
sub set_deployment_features($$$$$$);

# print and debug
sub list_node($);
sub debug_print($$);

# deployment time
sub set_time($$);
sub get_time($$);

# other tools #
sub correct_db_consistence($);
sub correct_deployment_consistence($$);
sub erase_partition($%);

#####################
# END OF PROTOTYPES #
#####################

###########################
### database connectors ###

# connect
# connects to database and returns the base identifier
# parameters : /
# return value : base
sub connect() {
    my $status = 1;

    my $host = conflib::get_conf("deploy_db_host");
    my $name = conflib::get_conf("deploy_db_name");
    my $user = conflib::get_conf("deploy_db_login");
    my $pwd  = conflib::get_conf("deploy_db_psswd");

    my $dbh = DBI->connect("DBI:mysql:database=$name;host=$host",$user,$pwd,{'PrintError'=>0}) or $status = 0;
    
    if($status == 0){
	print "ERROR : connection to database $name failed\n";
	print "ERROR : please check your configuration file\n";
	exit 0;
    }

    return $dbh;
}

# disconnect
# disconnect from database
# parameters : base
# return value : /
sub disconnect($) {
    my $dbh = shift;

    # Disconnect from the database.
    $dbh->disconnect();
}

### database connectors end ###
###############################

# prepare_deployment
# checks if there is already a waiting deployment
# if not, creates a new one (waiting,startdate,enddate)
# parameters : base
# return value : 
#  - deployment id if the new deployment has been successfully created
#  -      0        if not i.e. if there is already a waiting deployment
sub prepare_deployment($){
    my $dbh = shift;

    $dbh->do("LOCK TABLES deployment WRITE");

    my $sth = $dbh->prepare("SELECT IFNULL(COUNT(deployment.id),0) as id
                             FROM deployment
                             WHERE deployment.state = 'waiting'");
    $sth->execute();
    my $nb = $sth->fetchrow_hashref(); $nb = $nb->{'id'};
    $sth->finish();

    # $nb should be 0 or 1
    if($nb == 0){

	# create new deployment, set state to waiting and start_date to current mysql server date
	$sth = $dbh->do("INSERT deployment (state, startdate, enddate)
                         VALUES ('waiting', NOW(), NOW())");

	$sth = $dbh->prepare("SELECT deployment.id FROM deployment WHERE deployment.state = 'waiting'");
	$sth->execute();
	my $deploy_id = $sth->fetchrow_hashref();
	$deploy_id = $deploy_id->{'id'};
	$sth->finish();
 
	$dbh->do("UNLOCK TABLES");
	return $deploy_id;
    }elsif($nb == 1){
	print "ERROR : another deployment is already waiting ; please retry later\n";
	$dbh->do("UNLOCK TABLES");
	return 0;
    }else{
	print "ERROR : unexpected number of waiting deployment \"$nb\"\n";
	return 0;
    }
}

# run_deployment
# sets running deployment state
# prerequisite : begin_deployment
# parameters : base, deploy_id
# return value : 1 if successful, 0 if failed
sub run_deployment($$){
    my $dbh = shift;
    my $deploy_id = shift;

    # set current state to deploying
    my $rows_affected = $dbh->do("UPDATE deployment
                                  SET deployment.state = 'running'
                                  WHERE deployment.state = 'waiting'");
    if($rows_affected == 1){
	return 1;
    }else{
	print "unexpected number of waiting deployment\n";
	return 0;
    }
}

# end_deployment
# sets end deployment state i.e.
# - end state in deployment table
# - end state in depoyed table
# prerequisite : begin_deployment call
# parameters : base, deploy_id
# return value : /
sub end_deployment($$){
    my $dbh = shift;
    my $deploy_id = shift;
    my $failure = 0;
    my @node = ();

    $dbh->do("UPDATE deployed SET deployed.state = 'deployed'
              WHERE deployed.deployid = $deploy_id
              AND deployed.state = 'deploying'");

    my $sth = $dbh->prepare("SELECT count(deployed.nodeid) AS nb FROM deployed
                             WHERE deployed.deployid = $deploy_id
                             AND deployed.state <> 'deployed'");
    $sth->execute();
    my $undeployed = $sth->fetchrow_hashref();
    $undeployed = $undeployed->{'nb'};
    $sth->finish();

    if($undeployed){
	print "The deployment failed on some (maybe not all) nodes\n";
	$sth = $dbh->do("UPDATE deployment
                         SET deployment.state = 'error', deployment.enddate = NOW()
                         WHERE deployment.id = $deploy_id");
    }else{
	$sth = $dbh->do("UPDATE deployment
                         SET deployment.state = 'terminated', deployment.enddate = NOW()
                         WHERE deployment.id = $deploy_id");
    }
}

# cancel_deployment
# sets error state in deployed and deployment tables
# prerequisite : prepare_deployment call
# parameters : base, deploy_id
# return value : /
sub cancel_deployment($$){
    my $dbh = shift;
    my $deploy_id = shift;

    # set deployed state to error
    my $sth = $dbh->do("UPDATE deployed 
                        SET deployed.state = 'error' 
                        WHERE deployed.deployid = $deploy_id");

    deploy_iolib::end_deployment($dbh,$deploy_id);
}

############################################
### deployment partition selection tools ###

# get_partition_status
# gets the number and content of partitions on the specified node 
# whose size is higher than the one given in parameter
# parameters : base, minimal size, array of ip addresses
# return value : ip address, disk id, partition id, environment id
#                in a hash of arrays as decribed below :
#                %res = (
#                     ip_addr1 => ( 
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id]
#                                 );
#                     ip_addr2 => ( 
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id]
#                                 );
#                       ...
#                       );
sub get_partition_status($$@) {
    my $dbh = shift;
    my $size = shift;
    my @ip_addr = @_;
    
    my %res;
    my @res;

    foreach my $ip_addr (@ip_addr){
	my $sth = $dbh->prepare("SELECT deployed.diskid, deployed.partid, deployed.envid
	    		         FROM deployed, node, partition
	    		         WHERE node.ipaddr = \"$ip_addr\"
                                 AND partition.size >= $size
                                 AND node.id = deployed.nodeid
                                 AND partition.id = deployed.partid");
	$sth->execute();
	@res = ();
	
	while (my $ref = $sth->fetchrow_hashref()) {
	    push(@res, [$ref->{diskid}, $ref->{partid}, $ref->{envid}]);
	}
	$sth->finish();

	# warning
	if (!@res) {print "WARNING : $ip_addr has no suitable partition for requested size\n"}

	$res{$ip_addr} = [@res];
    }
    
    return %res;
}

# get_deploy_id
# returns the deploy_id for the specified partition if in an appropriate state
# parameters : base, node name, device, partition
# return value : deploy_id if state = 'deployed' else 0
sub get_deploy_id($$$$){
    my $dbh  = shift;
    my $node = shift;
    my $dev  = shift;
    my $part = shift;

    my $nodeid = node_name_to_id($dbh,$node);
    my $diskid = disk_dev_to_id($dbh,$dev);
    my $partid = part_nb_to_id($dbh,$part,$nodeid);

    # debug print
    # print "NODEID = $nodeid ; DISKID = $diskid ; PARTID = $partid\n";

    my $sth = $dbh->prepare("SELECT deployed.deployid
                             FROM deployed
                             WHERE deployed.nodeid = \"$nodeid\" 
                             AND deployed.diskid = \"$diskid\"
                             AND deployed.partid = \"$partid\"
                             AND deployed.state = 'deployed'");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    $ref = ($ref->{'deployid'});
    $sth->finish();

    return $ref;
}

# search_deployed_env
# looks for partitions with a given deployed environment on the given node
# parameters : base, environment name, node name
# return value : partition and matching environment version (if any)
#                in an array as described below :
#                @res = ([disk_id1, part_id1, env_version],
#                        [disk_id2, part_id2, env_version],
#                        ...)
sub search_deployed_env($$$){
    my $dbh = shift;
    my $env_name = shift;
    my $node_name = shift;
    my @res=();

    # debug print
    # print "ENV = $env_name ; NODE = $node_name\n";
    
    my $sth = $dbh->prepare("SELECT deployed.diskid, deployed.partid, environment.version
                             FROM node, deployed, environment
                             WHERE deployed.nodeid = node.id
                             AND deployed.state = 'deployed'
                             AND deployed.envid = environment.id
                             AND environment.name = \"$env_name\" 
                             AND node.name = \"$node_name\"");
    $sth->execute();
	
    while (my $ref = $sth->fetchrow_hashref()) {
	my @deployed = ($ref->{'diskid'},$ref->{'partid'},$ref->{'version'});
	push(@res,[@deployed]);
    }
    
    if(!scalar(@res)){
	print "WARNING : node $node_name has no partition with environment $env_name deployed on\n";
    }
    
    $sth->finish();

    return @res;
}

# autochoose_partition
# automatically chooses the first free partition on each node
# prerequisite : get_partition_status
# parameters : ip address, disk, partition, environment
#              in a hash of arrays as described below :
#              %param = (
#                     ip_addr1 => ( 
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id],
#                                              ...
#                                 );
#                     ip_addr2 => ( 
#                                   [disk id, part id, env id],
#                                   [disk id, part id, env id],
#                                              ...
#                                 );
#                       ...
#                       );
# return value : ip address, disk, partition
#                in a hash of arrays as described below :
#                %res = (
#                     ip_addr1 => ("disk id, part id");
#                     ip_addr2 => ("disk id, part id");
#                       ...
#                       );
sub autochoose_partition($%){
    my $dbh = shift;
    my %ip_addr = @_;
    my @candidate = ();
    my %res;

    # gets empty environment id
    my $sth = $dbh->prepare("SELECT environment.id FROM environment WHERE environment.name = 'empty'"); 
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $empty_env_id = $ref->{'id'};
    $sth->finish();

    for my $ip_addr (keys %ip_addr){
	my $i =0; 
	my $bsup = $#{$ip_addr{$ip_addr}};
	my $found = 0;
	while ($i <= $bsup && !$found){
	    if ($ip_addr{$ip_addr}[$i][2] == $empty_env_id){
		pop(@{$ip_addr{$ip_addr}[$i]});
		$res{$ip_addr} = $ip_addr{$ip_addr}[$i];
		$found = 1;
	    }
	    $i++;
	}
	if (!$found){
	    print "WARNING : no free partition found on node \"$ip_addr\"\n";
	}
    }
    return %res;
}

### deployment partition selection tools end ###
################################################

##########################
### database accessors ###

# env_name_ver_to_id
# gets the environment id matching the given name and version
# parameters : base, environment name, environment version
# return value : environment id
sub env_name_ver_to_id($$$){
    my $dbh = shift;
    my $name = shift;
    my $version = shift;

    my $sth = $dbh->prepare("SELECT environment.id
                             FROM environment 
                             WHERE environment.name = \"$name\" 
                             AND environment.version = \"$version\"");
    $sth->execute();
    my $id = $sth->fetchrow_hashref();
    $id = $id->{'id'};
    $sth->finish();
    
    if(!$id){
	print "WARNING : there is no environment matching $name $version\n";
	return 0;
    }else{
	return $id;
    }

}

# env_name_to_last_ver_id
# gets the id of the last version matching the given environment name
# parameters : base, environment name
# return value : environment id
sub env_name_to_last_ver_id($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $sth = $dbh->prepare("SELECT environment.id
                                 FROM environment 
                                 WHERE environment.name = \"$name\" 
                                 AND environment.version = \"$version\"");
	$sth->execute();
	my $id = $sth->fetchrow_hashref();
	$id = $id->{'id'};
	$sth->finish();
	return $id;
    }
}

# env_undefined_to_id
# gets the id of the special 'undefined' environment
# parameters : base
# return value : undefined environment id
sub env_undefined_to_id($){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT environment.id
                             FROM environment 
                             WHERE environment.name = 'undefined'");
    $sth->execute();
    my $id = $sth->fetchrow_hashref();
    $id = $id->{'id'};
    $sth->finish();

    if(!$id){
	print "ERROR : there is no undefined environment\n";
	print "ERROR : this shouldn't happen\n";
	return 0;
    }else{
	return $id;
    }
}

# env_name_to_versions
# gets the existing versions of a given environment
# parameters : base, environment name
# return value : array of environment versions
sub env_name_to_versions($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT environment.version
                             FROM environment 
                             WHERE environment.name=\"$name\"");
    $sth->execute();
    my @version = ();

    while (my $ref = $sth->fetchrow_hashref()) {
        push(@version, $ref->{'version'});
    }
    $sth->finish();
    
    if (!scalar(@version)){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }

    return @version;
}

# env_id_to_name
# gets the name matching the id environment
# parameters : base, environment id
# return value : environment name
sub env_id_to_name($$){
    my $dbh = shift;
    my $id = shift;

    print "YOP";

    my $sth = $dbh->prepare("SELECT environment.name
                             FROM environment 
                             WHERE environment.id = \"$id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $name = $ref->{'name'};
    $sth->finish();
    
    if (!$name){
	print "WARNING : there is no environment of id $id\n";
	return 0;
    }

    return $name;
}

# env_id_to_size
# gets the version matching the id environment
# parameters : base, environment id
# return value : environment version
sub env_id_to_size($$){
    my $dbh = shift;
    my $id = shift;

    my $sth = $dbh->prepare("SELECT environment.size
                             FROM environment 
                             WHERE environment.id = \"$id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $size = $ref->{'size'};
    $sth->finish();
    
    if (!$sth){
	print "WARNING : there is no environment of id $id\n";
	return 0;
    }

    return $size;
}

# env_name_to_filesystem
# gets the filesystem  for last ver of given env
# parameters : base, environment name
# return value : file system
sub env_name_to_filesystem($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $sth = $dbh->prepare("SELECT environment.filesystem
                                FROM environment
                                WHERE environment.name = \"$name\" 
                                AND environment.version = \"$version\"");
	$sth->execute();
	my $kernel = $sth->fetchrow_hashref();
	$kernel = $kernel->{'filesystem'};
	$sth->finish();
	return $kernel;
    }
}

# env_name_to_kernel
# gets the environment kernel path for last ver of given env
# parameters : base, environment name
# return value : kernel path
sub env_name_to_kernel($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $sth = $dbh->prepare("SELECT environment.kernelpath
                                FROM environment
                                WHERE environment.name = \"$name\" 
                                AND environment.version = \"$version\"");
	$sth->execute();
	my $kernel = $sth->fetchrow_hashref();
	$kernel = $kernel->{'kernelpath'};
	$sth->finish();
	return $kernel;
    }
}

# env_name_to_filebase
# gets the filebase path for last ver of given env
# parameters : base, environment name
# return value : filebase path
sub env_name_to_filebase($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $sth = $dbh->prepare("SELECT environment.filebase
                                 FROM environment
                                 WHERE environment.name = \"$name\" 
                                 AND environment.version = \"$version\"");
	$sth->execute();
	my $filebase = $sth->fetchrow_hashref();
	$filebase = $filebase->{'filebase'};
	$sth->finish();
	return $filebase;
    }
}

# env_name_to_size
# gets the size of the last ver of given env
# parameters : base, environment name
# return value : size
sub env_name_to_size($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $sth = $dbh->prepare("SELECT environment.size
                                 FROM environment
                                 WHERE environment.name = \"$name\" 
                                 AND environment.version = \"$version\"");
	$sth->execute();
	my $size = $sth->fetchrow_hashref();
	$size = $size->{'size'};
	$sth->finish();
	return $size;
    }
}

# env_name_to_filesite
# gets the filesite path for last ver of given env
# parameters : base, environment name
# return value : filesite path
sub env_name_to_filesite($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT MAX(environment.version) as max_version
                             FROM environment 
                             WHERE environment.name = \"$name\"");
    $sth->execute();
    my $version = $sth->fetchrow_hashref();
    $version = $version->{'max_version'};
    $sth->finish();

    if(!$version){
	print "WARNING : there is no environment named $name\n";
	return 0;
    }else{
	my $stg = $dbh->prepare("SELECT environment.filesite
                                 FROM environment
                                 WHERE environment.name = \"$name\" 
                                 AND environment.version = \"$version\"");
	$stg->execute();
	my $filesite = $stg->fetchrow_hashref();
	$filesite = $filesite->{'filesite'};
	$stg->finish();
	return $filesite;
    }
}

# disk_dev_to_id
# gets the id of the device dev
# parameters : base, device
# return value : disk id
sub disk_dev_to_id($$){
    my $dbh = shift;
    my $dev = shift;

    my $sth = $dbh->prepare("SELECT disk.id
                             FROM disk
                             WHERE disk.device = \"$dev\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $id = $ref->{'id'};
    $sth->finish();
    
    if (!$id){
	print "WARNING : there is no device $dev\n";
	return 0;
    }

    return $id;
}

# disk_id_to_dev
# gets the device matching the id
# parameters : base, disk id
# return value : device
sub disk_id_to_dev($$){
    my $dbh = shift;
    my $id = shift;

    my $sth = $dbh->prepare("SELECT disk.device
                             FROM disk
                             WHERE disk.id = \"$id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $dev = $ref->{'device'};
    $sth->finish();
    
    if (!$dev){
	print "WARNING : there is no disk of id $id\n";
	return 0;
    }

    return $dev;
}

# part_nb_to_id
# gets the id of the partition nb
# parameters : base, partition nb, nodeid
# return value : partition id
sub part_nb_to_id($$$){
    my $dbh = shift;
    my $nb = shift;
    my $nodeid = shift;

    my $sth = $dbh->prepare("SELECT partition.id FROM partition, deployed WHERE partition.pnumber = \"$nb\" and partition.id = deployed.partid and deployed.nodeid = \"$nodeid\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $id = $ref->{'id'};
    $sth->finish();
    
    if (!$id){
	print "WARNING : there is no partition $nb\n";
	return 0;
    }

    return $id;
}

# part_nb_to_size
# gets the size of the partition nb
# parameters : base, partition nb
# return value : partition size
sub part_nb_to_size($$){
    my $dbh = shift;
    my $nb = shift;

    my $sth = $dbh->prepare("SELECT partition.size FROM partition WHERE partition.pnumber = \"$nb\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $size = $ref->{'size'};
    $sth->finish();
    
    if (!$size){
	print "WARNING : there is no partition $nb\n";
	return 0;
    }

    return $size;
}

# part_id_to_size
# gets the partition size of partition id id
# parameters : base, partition id
# return value : partition size
sub part_id_to_size($$){
    my $dbh = shift;
    my $id = shift;

    my $sth = $dbh->prepare("SELECT partition.size FROM partition WHERE partition.id = $id");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $size = $ref->{'size'};
    $sth->finish();
    
    if (!$size){
	print "WARNING : there is no partition of id $id\n";
	return 0;
    }

    return $size;
}

# part_id_to_nb
# gets the partition nb of partition id id
# parameters : base, partition id
# return value : partition nb
sub part_id_to_nb($$){
    my $dbh = shift;
    my $id = shift;

    my $sth = $dbh->prepare("SELECT partition.pnumber FROM partition WHERE partition.id = \"$id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my $nb = $ref->{'pnumber'};
    $sth->finish();
    
    if (!$nb){
	print "WARNING : there is no partition of id $id\n";
	return 0;
    }

    return $nb;
}

# node_id_to_name
# gets the name of the node
# parameters : base, id
# return value : node name or 0 if it dose not exist in the database
sub node_id_to_name($$){
    my $dbh = shift;
    my $id = shift;

    my $sth = $dbh->prepare("SELECT node.name FROM node WHERE node.id = $id"); 
    $sth->execute(); 
    my $ref = $sth->fetchrow_hashref(); 
    my $name = $ref->{'name'}; 
    $sth->finish();

    if(!$name){
	return 0;
    }
    return $name;
}

# node_name_to_id
# gets the identifier of the node
# parameters : base, name
# return value : node id or 0 if it dose not exist in the database
sub node_name_to_id($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT node.id FROM node WHERE node.name = \"$name\""); 
    $sth->execute(); 
    my $ref = $sth->fetchrow_hashref(); 
    my $id = $ref->{'id'}; 
    $sth->finish();

    if(!$id){
	return 0;
    }
    return $id;
}

# node_name_to_ip
# gets the ip address of the node name
# parameters : base, name
# return value : ip address or 0 if it does not exist in the database
sub node_name_to_ip($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT node.ipaddr FROM node WHERE node.name = \"$name\""); 
    $sth->execute(); 
    my $ref = $sth->fetchrow_hashref(); 
    my $addr = $ref->{'ipaddr'}; 
    $sth->finish();

    if(!$addr){
	return 0;
    }
    return $addr;
}

# node_ip_to_name
# gets the name of the node whose ip address is given in parameter
# parameters : base, ip address
# return value : name or 0 if it does not exist in the database
sub node_ip_to_name($$){
    my $dbh = shift;
    my $ip = shift;
    
    my $sth = $dbh->prepare("SELECT node.name FROM node WHERE node.ipaddr = \"$ip\""); 
    $sth->execute(); 
    my $ref = $sth->fetchrow_hashref(); 
    my $name = $ref->{'name'}; 
    $sth->finish();

    if(!$name){
	return 0;
    }
    return $name;
}

# node_name_to_name
# checks if the node exists
# parameters : base, node name
# return value : node name or 0 if it does not exist in the database
sub node_name_to_name($$){
    my $dbh = shift;
    my $name = shift;

    my $sth = $dbh->prepare("SELECT node.name FROM node WHERE node.name = \"$name\""); 
    $sth->execute(); 
    my $ref = $sth->fetchrow_hashref(); 
    my $exist = $ref->{'name'}; 
    $sth->finish();

    if(!$exist){
	return 0;
    }
    return $exist;
}

# deploy_id_to_env_info
# returns 
# parameters : base, deploy id
# return value : name and kernel path of env to be deployed in an array (name, kernelpath)
sub deploy_id_to_env_info($$){
    my $dbh = shift;
    my $deploy_id = shift;

    my $sth = $dbh->prepare("SELECT environment.id, environment.name, environment.kernelpath, environment.kernelparam, environment.initrdpath FROM deployed, environment 
                             WHERE deployed.envid = environment.id AND deployed.deployid = \"$deploy_id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my @env_info = ($ref->{'name'},$ref->{'kernelpath'}, $ref->{'kernelparam'}, $ref->{'initrdpath'}, $ref->{'id'}); 
    $sth->finish();

    return @env_info;
}

# deploy_id_to_node_info
# returns 
# parameters : base, deploy id
# return value : 
sub deploy_id_to_node_info($$){
    my $dbh = shift;
    my $deploy_id = shift;
    my %node_info;

    my $sth = $dbh->prepare("SELECT node.ipaddr, disk.device, partition.pnumber FROM deployed, node, disk, partition
                             WHERE deployed.nodeid = node.id AND deployed.diskid = disk.id AND deployed.partid = partition.id AND deployed.deployid = \"$deploy_id\"");
    $sth->execute();
    while (my $ref = $sth->fetchrow_hashref()){
        $node_info{$ref->{'ipaddr'}} = [$ref->{'device'},$ref->{'pnumber'}];
    }
    $sth->finish();

    return %node_info;
}

### database accessors end ###
##############################

######################################
### kaaddnode/kadelnode tools ###

# add_node
# registers a new node into the database
# parameters : base, node_description
# return value : node id
sub add_node($$){
    my $dbh = shift;
    my $ref_node = shift;
    my $node_id;
    my $sth;
    
    # TODO : faire des vérifications !
    
    # debug print
    # print "VALUES = $$ref_node[0] ; $$ref_node[1] ; $$ref_node[2]\n";
    
    # checks if node already exists
    $sth = $dbh->prepare("SELECT node.id FROM node WHERE node.name = \"$$ref_node[0]\"");
    $sth->execute();
    $node_id = $sth->fetchrow_hashref(); 
    $node_id = $node_id->{'id'}; 
    $sth->finish();
    
    if(!$node_id){
	# enregistrer le nouveau noeud
	$sth = $dbh->do("INSERT node (name, macaddr, ipaddr)
                         VALUES (\"$$ref_node[0]\",\"$$ref_node[1]\",\"$$ref_node[2]\")");
	
	$sth = $dbh->prepare("SELECT node.id FROM node WHERE node.name = \"$$ref_node[0]\"");
	$sth->execute();
	$node_id = $sth->fetchrow_hashref(); 
	$node_id = $node_id->{'id'}; 
	$sth->finish();

    }
    return $node_id;
    
}

# add_deploy
# registers a deployment state
# parameters : base, reference to (part,env), disk id, host id
# return value : /
sub add_deploy($$$$){
    my $dbh = shift;
    my $ref_part_env = shift;
    my $disk_id = shift;
    my $host = shift;

    # debug print
    # print "REF : ($$$ref_part_env[0];$$$ref_part_env[1]) ; DID = $disk_id ; HOST = $host\n";

    my $sth = $dbh->do("INSERT deployed (envid, diskid, partid, nodeid, deployid, state)
                        VALUES ($$$ref_part_env[1],$disk_id,$$$ref_part_env[0],$host,0,'deployed')");
}

# add_partition
# registers partition
# parameters : base, partition_info, disk id
# return value : partition id
sub add_partition($$$){
    my $dbh = shift;
    my $ref_part = shift;
    my $disk_id = shift;
    my $part_id;

    # TODO : faire des vérifications !

    # debug print
    #print "VALUES = $$ref_part[0] ; $$ref_part[1] ; $$ref_part[2] ; DID $disk_id\n";

    # checks if partition type already exists
    my $sth = $dbh->prepare("SELECT partition.id FROM partition
                             WHERE partition.pnumber = \"$$ref_part[0]\"
                             AND partition.size = \"$$ref_part[1]\"");
    $sth->execute();
    $part_id = $sth->fetchrow_hashref(); 
    $part_id = $part_id->{'id'}; 
    $sth->finish();

    if(!$part_id){
	$sth = $dbh->do("INSERT partition (pnumber, size)
                         VALUES (\"$$ref_part[0]\",\"$$ref_part[1]\")");
        $sth = $dbh->prepare("SELECT partition.id FROM partition
                              WHERE partition.pnumber = \"$$ref_part[0]\"
                              AND partition.size = \"$$ref_part[1]\"");
	$sth->execute();
	$part_id = $sth->fetchrow_hashref(); 
	$part_id = $part_id->{'id'}; 
	$sth->finish();
    }
    return $part_id;
}

# add_disk
# registers disk
# parameters : base, disk_info
# return value : disk id
sub add_disk($$){
    my $dbh = shift;
    my $ref_disk = shift;
    my $id;

    # TODO : faire des vérifications !

    # debug print
    #print "VALUES = $$ref_disk[0] ; $$ref_disk[1]\n";

    # checks if disk type already exists
    my $sth = $dbh->prepare("SELECT disk.id FROM disk
                             WHERE disk.size = \"$$ref_disk[1]\" 
                             AND disk.device = \"$$ref_disk[0]\"");
    $sth->execute();
    $id = $sth->fetchrow_hashref(); 
    $id = $id->{'id'}; 
    $sth->finish();

    if(!$id){
	$sth = $dbh->do("INSERT disk (size, device)
                         VALUES (\"$$ref_disk[1]\",\"$$ref_disk[0]\")");
	$sth = $dbh->prepare("SELECT disk.id FROM disk
                              WHERE disk.size = \"$$ref_disk[1]\" 
                              AND disk.device = \"$$ref_disk[0]\"");
	$sth->execute();
	$id = $sth->fetchrow_hashref(); 
	$id = $id->{'id'}; 
	$sth->finish();
    }
    
    return $id;
}

# add_environment
# registers a new environment into the database
# parameters : base, name, version, description, author, filebase, filesite, size, initrdpath, kernelpath, kernelparam, fdisktype, filesystem, site id
# return value : 1 if successful, 0 otherwise
sub add_environment($$$$$$$$$$$$$$){
    my $dbh = shift;
    my $name = shift;
    my $version = shift;
    my $description = shift;
    my $author = shift;
    my $filebase = shift;
    my $filesite = shift;
    my $size = shift;
    my $initrdpath = shift;
    my $kernelpath = shift;
    my $kernelparam = shift;
    my $fdisktype = shift;
    my $filesystem = shift;
    my $siteid = shift;

    # debug print
    #print "$name , $version , $description , $author , $filebase , $filesite , $size , $initrdpath , $kernelpath , $kernelparam , $fdisktype , $filesystem , $siteid\n";
    
    my $sth = $dbh->do("SELECT environment.name, environment.version FROM environment
                        WHERE environment.name = \"$name\" AND environment.version = $version");

    if($sth == 1){
	print "ERROR : environment $name version $version already exists\n";
	return 0;
    }else{
	my $successful = 0;
	$successful = $dbh->do("INSERT environment (name,version,description,author,filebase,filesite,size,initrdpath,kernelpath,kernelparam,fdisktype,filesystem,siteid) VALUES (\"$name\",\"$version\",\"$description\",\"$author\",\"$filebase\",\"$filesite\",$size,\"$initrdpath\",\"$kernelpath\",\"$kernelparam\",\"$fdisktype\",\"$filesystem\",$siteid)");
	return $successful;
    }

}

# del_node
# delete a node from the database
# parameters : base, node
# return value : /
sub del_node($$){
    my $dbh = shift;
    my $host = shift;
    my $sth;
    
    $sth = $dbh->prepare("SELECT node.id FROM node WHERE node.name = \"$host\"");
    $sth->execute();
    my $node_id = $sth->fetchrow_hashref(); 
    $node_id = $node_id->{'id'}; 
    $sth->finish();
    
    if(!$node_id){
	print "WARNING : node $host to delete is not registered\n";
    }else{
	$sth = $dbh->do("DELETE FROM deployed WHERE deployed.nodeid = $node_id");
	$sth = $dbh->do("DELETE FROM node WHERE node.id = $node_id");
    }
}

### kaaddnode/kadelnode operations end ###
##########################################

#######################
### print and debug ###

# list_node
# gets the list of all node.
# parameters : base
# return value : list of hostnames
# side effects : /
sub list_node($) {
    my $dbh = shift;
    my $sth = $dbh->prepare("SELECT * FROM node");
    $sth->execute();
    my @res = ();

    while (my $ref = $sth->fetchrow_hashref()) {
        push(@res, $ref->{'name'});
    }
    $sth->finish();
    return @res;
}

# debug_print
# prints database state 
# parameters : base, deploy_id
# return value : /
sub debug_print($$){
    my $dbh = shift;
    my $deploy_id = shift;
    my %res;

    # gets interesting information

    # from deployed table
    my $sth = $dbh->prepare("SELECT deployed.nodeid, deployed.state, deployed.error_description 
                             FROM deployed WHERE deployed.deployid = \"$deploy_id\"");
    $sth->execute();
    while (my $ref = $sth->fetchrow_hashref()) {
	$res{$ref->{'nodeid'}} = [$ref->{'state'},$ref->{'error_description'}];
    }
    $sth->finish();

    # from deployment table
    $sth = $dbh->prepare("SELECT deployment.id, deployment.state 
                          FROM deployment 
                          WHERE deployment.id = \"$deploy_id\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref();
    my @depl = ($ref->{'id'},$ref->{'state'});
    $sth->finish();

    # prints information
    print "\nDeploy\tState\n";
    print "------\t-----\n";
    print "$depl[0]\t$depl[1]\n";

    print "\nNode\tState\t\tError Description (if any)\n";
    print "----\t-----\t\t--------------------------\n";
    foreach my $res (keys %res){
	if ($res{$res}[0] eq 'error'){
	    print "$res\t$res{$res}[0]\t\t$res{$res}[1]\n";
	}else{
	    print "$res\t$res{$res}[0]\t\n";
	}
    }
    print "\n";
}

### print and debug end ###
###########################

#######################
### deployment time ###

# set_time
# sets end deployment time to current date
# parameters : base, deployment id
# return value : /
sub set_time($$){
    my $dbh = shift;
    my $id = shift;
    my $sth;

    $sth = $dbh->do("UPDATE deployment 
                     SET deployment.enddate = NOW() 
                     WHERE deployment.id = $id");
}

# get_time
# returns time difference between current date and end deployment date
# parameter : base, deployment id
# return value : time difference between current date and end date
sub get_time($$){
    my $dbh = shift;
    my $id = shift;
    my $sth;

    $sth = $dbh->prepare("SELECT (TIME_TO_SEC(NOW()) - TIME_TO_SEC(enddate)) AS diff
                          FROM deployment WHERE deployment.id = $id");
    $sth->execute();
    my $diff = $sth->fetchrow_hashref();
    $diff = $diff->{'diff'};
    $sth->finish();

    return $diff;
}

### deployment time end ###
###########################

# report_state
# report the state of the node into the database after a check
# parameters : base, deployment, name, state, error status
# return value : /
sub report_state($$$$$){
    my $dbh = shift;
    my $deploy_id = shift;
    my $name = shift;
    my $state = shift;
    my $error_status = shift;
    
    my $sth = $dbh->prepare("SELECT node.id FROM node WHERE node.name = \"$name\"");
    $sth->execute();
    my $ref = $sth->fetchrow_hashref(); 
    my $id = $ref->{'id'}; 
    $sth->finish();
    $sth = $dbh->do("UPDATE deployed 
                     SET deployed.state = \"$state\", deployed.error_description = \"$error_status\"
                     WHERE deployed.nodeid = $id AND deployed.deployid = $deploy_id");
}

# is_node_free
# checks if the node is free for deployment
# parameters : base, node id, deployment id
# return value : 1 if node is free, otherwise 0 
sub is_node_free($$$){
    my $dbh = shift;
    my $node_id = shift;
    my $deploy_id = shift;
    my $sth;

    # debug print
    # print "NODEID = $node_id ; DEPLOYID = $deploy_id\n";

    # checks if node is already involved in another running deployment
    $sth = $dbh->prepare("SELECT IFNULL(COUNT(deployed.nodeid),0) as id
	                  FROM deployed, deployment
	                  WHERE deployed.nodeid = $node_id
                          AND deployment.id = deployed.deployid
		          AND deployment.state = 'running'");
    $sth->execute();
    my $nb = $sth->fetchrow_hashref(); 
    $nb = $nb->{'id'};
    $sth->finish();
    if($nb == 0){
	# node is free
	return 1;
    }elsif($nb == 1){
	my $node = deploy_iolib::node_id_to_name($dbh,$node_id);
	print "WARNING : node $node is already involved in another deployment\n";
	return 0;
    }else{
	print "ERROR : unexpected number of partition involved in deployment for node ($nb)\n";
	return 0;
    }
}

# add_node_to_deployment
# performs several checks and do db appropriate modifications
# parameters : base, hostname, deploy id, env name, disk dev, part nb
# return value : 1 if successful, 0 if failed
sub add_node_to_deployment($$$$$$){
    my $dbh = shift;
    my $hostname = shift;
    my $deploy_id = shift;
    my $env_name = shift;
    my $disk_dev = shift;
    my $part_nb = shift;

    my $node_id = deploy_iolib::node_name_to_id($dbh,$hostname);
    my $disk_id = deploy_iolib::disk_dev_to_id($dbh,$disk_dev);
    my $part_id = deploy_iolib::part_nb_to_id($dbh,$part_nb,$node_id);
    my $env_id  = deploy_iolib::env_name_to_last_ver_id($dbh,$env_name);
    
    # performs cheks
    if((!$node_id) || (!$disk_id) || (!$part_id) || (!$env_id)){
	return 0;
    }

    # ! debug print !
    # print "DEBUG : NODE = $node_id ; DISK = $disk_id ; PART = $partition_id ; ENV = $env_id\n";

    my $is_free = deploy_iolib::is_node_free($dbh,$node_id,$deploy_id);
    if($is_free){
	my $env_size  = deploy_iolib::env_id_to_size($dbh,$env_id);
	my $part_size = deploy_iolib::part_id_to_size($dbh,$part_id);
	if($env_size < $part_size){
	    deploy_iolib::set_deployment_features($dbh,$node_id,$deploy_id,$env_id,$disk_id,$part_id);
	}else{
	    return 0;
	}
    }else{
	return 0;
    }
    return 1;

}

# set_deployment_features
# sets deployment id in deployed table
# parameters : base, node id, deploy id, env id, disk id, part id
# return value : 1 if successful
sub set_deployment_features($$$$$$){
    my $dbh = shift;
    my $node_id = shift;
    my $deploy_id = shift;
    my $env_id = shift;
    my $disk_id = shift;
    my $part_id= shift;

    my $sth = $dbh->do("UPDATE deployed
                     SET deployed.envid = $env_id,
                         deployed.state = 'deploying',
                         deployed.deployid = $deploy_id
                     WHERE deployed.nodeid = $node_id
                     AND deployed.diskid = $disk_id
                     AND deployed.partid = $part_id");

    return 1;
}	  

###################
### other tools ###

# correct_db_consistence
# corrects the db in case it would be left in a inconsistent state
# parameters : base
# return value : /
# NB : first version very basic ; to be improved... ?
#      mustn't be used concurrently to deployment pocedures
sub correct_db_consistence($){
    my $dbh = shift;
    
    my $sth = $dbh->do("UPDATE deployed 
                        SET deployed.state='error', 
                            deployed.error_description='Auto-modified by Correct_db_consistence function' 
                        WHERE deployed.state='deploying' 
                        OR deployed.state='to_deploy'");
    $sth = $dbh->do("UPDATE deployment 
                     SET deployment.state='error', 
                         deployment.enddate=NOW() 
                     WHERE deployment.state='waiting' 
                     OR deployment.state='running'");
}

# correct_deployment_consistence
# corrects the db in case the deployment would be left in a inconsistent state 
# parameters : base, deployment
# return value : /
sub correct_deployment_consistence($$){
    my $dbh = shift;
    my $deploy_id = shift;
    
    my $sth = $dbh->do("UPDATE deployed 
                        SET deployed.state='error', 
                            deployed.error_description='Auto-modified by Correct_db_consistence function' 
                        WHERE deployed.deployid=$deploy_id
                        AND (deployed.state='deploying' OR deployed.state='to_deploy')");

    $sth = $dbh->do("UPDATE deployment 
                     SET deployment.state='error', 
                         deployment.enddate=NOW() 
                     WHERE deployment.id=$deploy_id
                     AND (deployment.state='waiting' OR deployment.state='running')");
}

# erase_partition
# changes deployed environment to empty on specified partition
# N.B. : it is also directly possible to deployed over a partition
# that already contains an environment via add_deployment_partition
# that will overwrite exisiting data
# parameters : base, ip and partition in a hash
#              structure of hash is described below :
#              %param = (
#                   ip_addr1 => ("disk id, part id");
#                   ip_addr2 => ("disk id, part id");
#                           ....
#                     );
# return value : /
sub erase_partition($%){
    my $dbh = shift;
    my %hash = @_;

    for my $to_erase (keys %hash){
	# gets node id
	my $sth = $dbh->prepare("SELECT node.id FROM node WHERE node.ipaddr = \"$to_erase\"");
	$sth->execute();    
	my $node_id = $sth->fetchrow_hashref();
	$node_id = $node_id->{'id'};
	$sth->finish();

	# gets empty environment id
	$sth = $dbh->prepare("SELECT environment.id FROM environment WHERE environment.name='empty'"); 
	$sth->execute();
	my $empty_env_id = $sth->fetchrow_hashref();
	$empty_env_id = $empty_env_id->{'id'};
	$sth->finish();

	$sth = $dbh->do("UPDATE deployed
                         SET deployed.envid = \"$empty_env_id\",
                             deployed.state = 'deployed',
                             deployed.deployid = '0'
                         WHERE deployed.nodeid = $node_id
                         AND deployed.diskid = $hash{$to_erase}[0]
                         AND deployed.partid = $hash{$to_erase}[1]");
    }
}

### other tools end ###
#######################

# END OF THE MODULE
return 1;
