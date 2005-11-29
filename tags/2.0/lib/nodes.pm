## operations on sets of nodes
package Nodes;

use strict;
use warnings;

# needed here
use IPC::Open3;

##
# Configuration Variables
##

my $nmapCmd = conflib::get_conf("nmap_cmd");
my $useNmapByDefault = conflib::get_conf("enable_nmap");
my $nmapArgs = conflib::get_conf("nmap_arguments"); # can add parameters to customize ping request to the site


# perl is shit....
#
my %parallel_launcher = (
    deployment => {
	sentinelleCmd => conflib::get_conf("deploy_sentinelle_cmd"),
	sentinelleDefaultArgs => conflib::get_conf("deploy_sentinelle_default_args"),
	sentinellePipelineArgs => conflib::get_conf("deploy_sentinelle_pipelined_args"),
	sentinelleEndings => conflib::get_conf("deploy_sentinelle_endings"),
	sentinelleTimeout => conflib::get_conf("deploy_sentinelle_timeout"),
    },
    production => { # only used for testing purposes for the moment...
	sentinelleCmd => conflib::get_conf("prod_sentinelle_cmd"),
	sentinelleDefaultArgs => conflib::get_conf("prod_sentinelle_default_args"),
	sentinellePipelineArgs => conflib::get_conf("prod_sentinelle_pipelined_args"),
	sentinelleEndings => conflib::get_conf("prod_sentinelle_endings"),
     	sentinelleTimeout => conflib::get_conf("prod_sentinelle_timeout"),
    },
);

##
# WARNING!!!
# sentinelle command must remain global variables, because you can't run commands
# on mixes of Nodes environment!!!
# should be done into separate calls!!
##

##
# Class Variables
##
my $environment; # used to check previous environment value when multiple set of nodes are created (cf new)
my $sentinelleCmd;
my $sentinelleDefaultArgs; # le timeout reporte les noeuds comme morts, si ,timeout=...
my $sentinellePipelineArgs; # sentinelle arguments for efficient data transmission
my $sentinelleEndings; # command at the end, should return host IP only on the target nodes!
my $sentinelleTimeout; # important because buggy sentinelle (return segfault...) should be timedout... after an answer! Another point is when you can ping a node and the rshd daemon is not yet launched.

##
# Others Variables
##
my $errorMessageOnCheck = "Not there on Check"; # error message for nodes that are reported dead after check

## PID of sentinelle
my $sentinellePID = 0; # pid of the current sentinelle process
my $userKilled = 0; # to determine wether sentinelle was killed on user demand or on alarm timeout

## Nodes constructor
sub new {
    my ($class, $env) = @_; # environment is production or deployment 
    my $self = {};

    if(!defined($parallel_launcher{$env})) {
	print "environment is not defined on Nodes creation, please refers configuration!\n";
	return 0;
    }
    if(!$environment) { # environment is not defined
	$environment = $env;
    } elsif ($environment ne $env) {
	print "environments should not be mixed in the same deployment!\n";
	return 0;
    }

    # initialize commands for the selected environment
    $sentinelleCmd = $parallel_launcher{$env}{sentinelleCmd};
    $sentinelleDefaultArgs = $parallel_launcher{$env}{sentinelleDefaultArgs};
    $sentinellePipelineArgs = $parallel_launcher{$env}{sentinellePipelineArgs};
    $sentinelleEndings = $parallel_launcher{$env}{sentinelleEndings};
    $sentinelleTimeout = $parallel_launcher{$env}{sentinelleTimeout};

    ###
    # TODO: checks should go into configuration checks!!
    ###
    ## sentinelle MUST be there
    #if (!defined($sentinelleCmd)){
 	#print "sentinelle not defined or not installed on your system\n";
	#return -1;
    #}
    ## want the help of nmap ?
    $self->{useNmap} = $useNmapByDefault;
    print "nmapCmd: $nmapCmd\n"; 
    if (!defined($nmapCmd) || (! -x $nmapCmd)){
	print "WARNING: nmap won't be used there\n";
	$self->{useNmap} = 0;
    }
    $self->{nodesByIPs} = {}; # hash of 'node' instances, key is IP
    $self->{nodesByNames} = {}; # hash of 'node' instances, key is hostname
    $self->{nodesNumber} = 0;
    $self->{nodesToPing} = []; # all nodes in an array
    $self->{nodesPinged} = []; # nodes after nmap
    $self->{nodesReady} = {}; # nodes ready (after check, or runCommand)
    $self->{nodesNotReached} = []; # nodes not reached by a parallel command (after check, or runCommand)
    $self->{commandSummary} = ""; # STDOUT summary of the last parallel command
    bless ($self, $class);
    return $self;
}

## kills the current sentinelle process
sub kill_sentinelle {
    if ($sentinellePID != 0) {
	print "kill sentinelle!!\n";
	$userKilled = 1;
	if (kill 0 => $sentinellePID) {
	    print "I can kill sentinelle\n";
	    kill 9,  $sentinellePID;
	}
    }
    return;
}

## discards a set of nodes, it is not a destructor
## it allows to verify that nodes subsets are under the same environment
## and when a set is discarded, that you can build a new one in another environment
sub discard {
    $environment = "";
}


## add a node in hash, no duplicate entry allowed 
sub add {
    my $self = shift;
    my $node = shift;
    my $nodeIP = $node->get_IP();
    my $nodeName = $node->get_name();

    if (!$nodeIP) {
	print "[Nodes::add] node $nodeName has no IP, not added to Nodes instance\n";
	return ;
    }
    $self->{nodesByIPs}->{$nodeIP}=$node; # perl uses references to instances, every hash refers to the same node instance
    $self->{nodesByNames}->{$nodeName}=$node;
    # if we use nmap, nodesPinged is discarted anyway
    push(@{$self->{nodesToPing}}, $nodeIP);
    push(@{$self->{nodesPinged}}, $nodeIP);

    $self->{nodesNumber} += 1;
}



#
# update the nodesReady hash, and the nodesNotReached array depending on Nodes' states
#
# Use it before manipulating these structures, because different sets of nodes can update their state!
#
# return the number of ready nodes
#
sub syncNodesReadyOrNot {
    my $self = shift;
    my $nodeIP;
    my $nodesReadyNumber = 0;

    # update the nodesReady hash, and the nodesNotReached array
    $self->{nodesReady} = {};
    $self->{nodesNotReached} = [];
    foreach $nodeIP (@{$self->{nodesToPing}}) {
	if($self->{nodesByIPs}->{$nodeIP}->get_state() == 1) {
 	    $self->{nodesReady}->{$nodeIP} = $self->{nodesByIPs}->{$nodeIP};
	    $nodesReadyNumber += 1;
	} else {
	    $self->{nodesByIPs}->{$nodeIP}->set_state(-1);
	    push(@{$self->{nodesNotReached}}, $nodeIP);
	}
    }
    return $nodesReadyNumber;
}



sub ready {
    my $self = shift;
    return ($self->syncNodesReadyOrNot() == $self->{nodesNumber}); 
}


sub getReadyNodes {
    my $self = shift;
    my $nodeName;
    my @result;
    $self->syncNodesReadyOrNot(); # sync Nodes' states
    foreach my $key (sort keys %{$self->{nodesReady}}) {
	$nodeName = $self->{nodesReady}->{$key}->get_name();
	push(@result, $nodeName);
    }
    return (@result);
}


sub getFailedNodes {
    my $self = shift;
    my $nodeName;
    my @result;
    $self->syncNodesReadyOrNot(); # sync Nodes' states
    foreach my $nodeIP (@{$self->{nodesToPing}}) {
	$nodeName = $self->{nodesByIPs}->{$nodeIP}->get_name();
	push(@result, $nodeName);
    }
    return (@result);
}


# get_node_by_name
#
#
sub get_node_by_name {
    my $self = shift;
    my $nodeName = shift;
    print "You want this node: $nodeName \n";
    if (exists($self->{nodesByNames}->{$nodeName})) {
	return ($self->{nodesByNames}->{$nodeName});
    }
    return 0;
}


# get_node_by_IP
#
#
sub get_node_by_IP {
    my $self = shift;
    my $nodeIP = shift;
    print "You want this node: $nodeIP \n";
    if (exists($self->{nodesByIPs}->{$nodeIP})) {
	return ($self->{nodesByIPs}->{$nodeIP});
    }
    return 0;
}



# getCommandSummary
#
# return the summary of the last parallel command
sub getCommandSummary {
    my $self = shift;
    return ($self->{commandSummary});
}

# reset commandSummary (should be used after a parallel command)
sub resetCommandSummary {
    my $self = shift;
    $self->{commandSummary} = "";
}


#
# Allows to report error on events that occurs to nodes
#
sub setNodesErrorMessage {
    my $self = shift;
    my $errorMessage = shift; # error message to report after a set_state

    foreach my $nodeIP (@{$self->{nodesToPing}}) {
	$self->{nodesByIPs}{$nodeIP}->set_error($errorMessage);
    }    
}


# checkNmap
#
# fills $self->{nodesPinged} with hosts'IPs
sub checkNmap {
    my $self = shift;
    my $command = "sudo " . $nmapCmd . " -sP ";
    my $commandArgs = join(" ",@{$self->{nodesToPing}});
    my $pingedIP;
    $self->{nodesPinged} = []; # should be reset when using nmap or multiple node's occurrences appear
 
    open(PINGSCAN, $command.$commandArgs." |") or die ("[checkNmap] can't reach nmap output");
    while(<PINGSCAN>) {
	$_ =~ /\((\d+\.\d+\.\d+\.\d+)\)/ and push(@{$self->{nodesPinged}}, $1);
    }
    close(PINGSCAN);
    return scalar(@{$self->{nodesPinged}});
}


#
# Check nodes that are there and then change the state of the nodes that did not respond
#
sub check {
    my $self = shift;
    my $nodeIP;
    my $nodeState;
    my $nodesNumber = scalar(@{$self->{nodesToPing}});
    my $checkCommand = $sentinelleCmd;
    my $timedout;

    # set error message before check
    $self->setNodesErrorMessage("Not there on check!");
    # reset the nodesReady hash
    $self->{nodesReady} = {};


    if($nodesNumber == 0) { # nothing to do, so why get any further?	
	return 1;
    }

    if($self->{useNmap}) { # let's perform a first check
	$nodesNumber = $self->checkNmap();
	if($nodesNumber == 0) { # nothing to do after nmap, so why get any further?
	    # set the state of the disappeared nodes
	    foreach $nodeIP (@{$self->{nodesToPing}}) {
		$self->{nodesByIPs}{$nodeIP}->set_state(-1);
	    }
	    # Let's sync the structures 
	    $self->syncNodesReadyOrNot();
	    return 1;
	}
    }

    $checkCommand .= " " . $sentinelleDefaultArgs . " -m".join(" -m",@{$self->{nodesPinged}});
    if (defined($sentinelleEndings)) {
	$checkCommand .= " -- ".$sentinelleEndings;
    }
    #print $checkCommand . "\n";
    eval {
	$timedout = 0;
        $SIG{ALRM} = sub { $timedout = 1; die("alarm\n") };
        alarm($sentinelleTimeout);

        my $pid = open3(\*WRITER, \*READER, \*ERROR, $checkCommand );
	$sentinellePID = $pid; ## allows to kill sentinelle externally
        while(<READER>){
	    chomp($_);
            if ($_ =~ m/^\s*(\d+\.\d+\.\d+\.\d+)\s*$/m) {
		$nodeIP = $1;
		print "there on check:  $nodeIP \n";
		if(exists($self->{nodesByIPs}->{$nodeIP})) {
		    $self->{nodesByIPs}->{$nodeIP}->set_state(1);
		    $self->{nodesReady}->{$nodeIP} = $self->{nodesByIPs}->{$nodeIP};
		} else { # this should be a big trouble!!
		    print "oups, here comes an unregistered node $nodeIP\n";
		}
	    }
	}
	waitpid($pid, 0);
	close(WRITER);
	close(READER);
        close(ERROR);
        alarm(0);
    };
    if ($@){
	if ($timedout == 0) {
	    print "killed by user...exiting\n";
	    exit 0;   
	}
        print("[Check] sentinelle command times out : all nodes are rebooting now\n");
	# We discard the results...
	$self->{nodesReady} = {}; 
    }

    # set the state of the disappeared nodes
    foreach $nodeIP (@{$self->{nodesToPing}}) {
	if (!exists($self->{nodesReady}->{$nodeIP})) {
	    $self->{nodesByIPs}{$nodeIP}->set_state(-1);
	}
    }

    # Let's sync the structures 
    $self->syncNodesReadyOrNot();
    return(1);
}




###                                                   ###
# Parallel command launchers with different subtilities #
###                                                   ###

#
# runs a command on a set of nodes, only the on the ones which are Ready.
# Initial state must be set through the check method on the set that contains all the nodes.
#
# Nodes that do not respond are reported in the nodesNotReached array
#
# returns values: 0 if a single node disappears
#
sub runIt {
    my $self = shift;
    my $executedCommand = shift; # parallel command launcher
    my $commandLocal = shift; # command run locally expl: tar -zxf image.tgz |
    my $commandRemote = shift; # command to execute on a set of nodes
    my $nodeIP;
    my $nodeState;
    my $nodesReadyNumber = $self->syncNodesReadyOrNot();
    my $return_value = 1;

    if($nodesReadyNumber == 0) { # no node is ready, so why get any further?
	return 1;
    }
    
    foreach my $key (sort keys %{$self->{nodesReady}}) {
	$executedCommand .= " -m$key";
    }
    $executedCommand .= " -- " . $commandRemote;

#print $executedCommand .  "\n";
    
    my $pid = open3(\*WRITER, \*READER, \*READER, $executedCommand );
    $sentinellePID=$pid;
    while(<READER>){
	chomp($_);
	if ($_ =~ m/^\s*(\d+\.\d+\.\d+\.\d+)\s*$/m) {
	    $nodeIP = $1;
	    if(exists($self->{nodesByIPs}->{$nodeIP})) {
		$nodeState = $self->{nodesByIPs}->{$nodeIP}->get_state();
		$self->{nodesByIPs}->{$nodeIP}->set_state(-1);
		if($nodeState == 1) { # node disappeared => update the structures
		    $return_value = 0;
		}
	    } else { # this should be a big trouble!!
		print "oups, node $nodeIP was not here while said to be ready!\n";
	    }
	}
	else  { # distinguished thanks to -v option in sentinelle
	    # collect STDOUT to $self->{commandSummary}
	    $self->{commandSummary} .= "$_\n";
	}
    }
    waitpid($pid, 0);
    close(WRITER);
    close(READER);

    # Let's sync the Nodes' state
    $self->syncNodesReadyOrNot();

    return $return_value;
}


#
# runs local and then parallel commands
# for non optimal, but safe pipelines
#
sub runCommand {
    my $self = shift;
    my $commandLocal = shift; # command run locally expl: tar -zxf image.tgz |
    my $commandRemote = shift; # command to execute on a set of nodes
    my $parallelLauncher = $commandLocal . " " . $sentinelleCmd . " " . $sentinelleDefaultArgs . " -v ";

    return ($self->runIt($parallelLauncher, $commandLocal, $commandRemote));
}

#
# runs local command and use mput for copy
#
sub runCommandMput {
    my $self = shift;
    my $remoteNamedPipe = "-p " . shift; # remote named pipe targetted with mput -p option
    my $parallelLauncher = "/usr/local/bin/mput" . " " . $sentinelleDefaultArgs . " -v ";

    return ($self->runIt($parallelLauncher, "", $remoteNamedPipe));
}



#
# runs the remote command only!
#
sub runRemoteCommand {
    my $self = shift;
    my $command = shift;
    return ($self->runCommand("", $command));
}


#
# evident names...
#
sub runReportedCommand {
    my $self = shift;
    my $commandLocal = shift; # command run locally expl: tar -zxf image.tgz |
    my $commandRemote = shift; # command to execute on a set of nodes
    my $errorMessage = shift; # error message to report

    $self->setNodesErrorMessage($errorMessage);
    return ($self->runCommand($commandLocal, $commandRemote));
}


sub runReportedRemoteCommand {
    my $self = shift;
    my $command = shift;
    my $errorMessage = shift; # error message to report

    $self->setNodesErrorMessage($errorMessage);
    return ($self->runRemoteCommand($command));
}


#
# launches a pipelined transfert, checks are crucial here!
# because an error breaks the pipe!
#
# Should only be used on efficients transferts for the moment!!
#
sub runEfficientPipelinedCommand {
    my $self = shift;
    my $commandLocal = shift; # command run locally expl: tar -zxf image.tgz |
    my $commandRemote = shift; # command to execute on a set of nodes
    my $errorMessage = shift; # error message to report on failure

    if(!$sentinellePipelineArgs) { # here, use runReportedCommand instead, not crucial anymore...
	return ($self->runReportedRemoteCommand($commandLocal, $commandRemote, $errorMessage))
    }
    my $parallelLauncher = $commandLocal . " " . $sentinelleCmd . " " . $sentinellePipelineArgs . " -v ";

    # up to now, every error means dead of the deployment
    $self->setNodesErrorMessage($errorMessage);
  
    if($self->runIt($parallelLauncher, $commandLocal, $commandRemote)) {
	return 1;
    }

    # here, we should discard all the nodes...
    # I am not sure the program will reach this part in case of a problem
    # if sentinelle hangs-> nothing more can be done...
    # everything that follows seems to be useless...
    foreach my $nodeIP (@{$self->{nodesToPing}}) {
	    $self->{nodesByIPs}{$nodeIP}->set_state(-1);
    }
    # Let's sync the Nodes' state
    $self->syncNodesReadyOrNot();

    return 0;
}


#
# Runs a remote system command and kill it after sentinelleTimeout
#
sub runRemoteSystemCommand {
    my $self = shift;
    my $executedCommand = "screen -d -m ". $sentinelleCmd . " " . $sentinelleDefaultArgs;
    my $commandRemote = shift; # command to execute on a set of nodes
    my $nodesReadyNumber = $self->syncNodesReadyOrNot();
    my $return_value = 1;
    my $timedout;
    my $pid;

    if($nodesReadyNumber == 0) { # no node is ready, so why get any further?
	return 1;
    }
    
    foreach my $key (sort keys %{$self->{nodesReady}}) {
	$executedCommand .= " -m$key";
    }
    $executedCommand .= " -- " . $commandRemote;

    #print $executedCommand;

    if (!defined($pid = fork())) {
	die "cannot fork: $!";
    } elsif ($pid) {
	# I'm the parent
	print "Forking child $pid\n";
    } else {
	exec ("$executedCommand") or die "Couldn't execute $executedCommand: $!\n";
    }
    # waits a little...
    sleep(2*$sentinelleTimeout);
    # kill the job!!!
    kill(9, $pid);

    #system($executedCommand);
    return 1;
}



1;
