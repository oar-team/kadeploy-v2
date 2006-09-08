package libkadeploy2::parallelcommand;

use strict;
use warnings;

#parallelLauncher : internalsafe
#                   internalwindow
#                   DKsentinelle

#$connector,$login,$timeout,$parallelLauncher
sub new($$)
{
    my $timeout=shift;
    my $verbose=shift;
    my $self;

     $self = 
    {
	"timeout"          => $timeout,
	"verbose"          => $verbose,
    };
    bless $self;
    return $self;
}


#param1: \@listcommand 
#return: \@commandResult
sub execsequential($)
{
    my $self=shift;
    my $listcommand=shift;
    my $command;
    my $retcode;
    my $timeout=2;
    my $i;
    my $ok=1;
    my @commandlist;
    @commandlist=@$listcommand;
    for ($i=0;$i<$#commandlist+1;$i++)
    {
	 my $pid = fork();
	 if ($pid == 0)
	 {
	     alarm($timeout);
	     exec($commandlist[$i]);
	     exit 1;
	 }
	 else
	 {
	      my $waited_pid = wait();
	      my $exit_value = $? >> 8;
	      my $signal_num  = $? & 127;
	      my $dumped_core = $? & 128;
	      $commandlist[$i]=$exit_value;
	      if ($exit_value!=0) { $ok=0; }
	 }
    }
    return $ok;
}

#
# runs an array of timedout commands
#
# %commandToRun: hash of already built commands to run  (host => command)
# timeout: timeout for the commands
# window_size: max number of simultaneous processes
# $errorString: error to register for the nodes on failure
#
sub execparallel($)
{
    my $self = shift;
    my $ref_to_commands = shift;
    my $timeout = $self->{timeout};
    my $window_size = 2;
    my $errorString = "toto";
    my $verbose=$self->{verbose};


    my @commandsToRun = @$ref_to_commands;
    my %commandRuned;
    my $ref;
    my @nodes=@commandsToRun;

    my $index = 0;
    my %running_processes;
    my %finished_processes;
    my $ok=1;

    if (!$window_size) {
	$window_size = scalar(@nodes)+1;
    }

    while (scalar(keys(%finished_processes)) <= $#nodes)
    {
	while((scalar(keys(%running_processes)) < $window_size) && 
	      ($index <= $#nodes))
	{
	    print("[VERBOSE] fork process for the $nodes[$index]\n") if ($verbose);
	    my $pid = fork();
	    if (defined($pid)){
		if ($pid == 0){
		    #In the child
		    # Initiate timeout
		    alarm($timeout);
		    my $cmd = $commandsToRun[$index];
		    print("[VERBOSE] Execute command : $cmd\n") if ($verbose);
		    if ($verbose)
		    {
			exec($cmd);
		    }
		    else
		    {
			exec($cmd." 2>&1 > /dev/null");
		    }
		}
		$running_processes{$pid} = $index;
		print ("[VERBOSE] job $pid forked\n") if ($verbose);
	    }
	    else
	    {
		warn("/!\\ fork system call failed for $nodes[$index].\n");
	    }
	    $index++;
	}
	my $waited_pid = wait();
	my $exit_value = $? >> 8;
	my $signal_num  = $? & 127;
	my $dumped_core = $? & 128;
	
	if ($waited_pid == -1)
	{
	    die("/!\\ wait return -1 so there is no child process. It is a mistake\n");
	}
	else
	{
	    if (defined($running_processes{$waited_pid})){
		print("[VERBOSE] Child process $waited_pid ended : exit_value = $exit_value, signal_num = $signal_num, dumped_core = $dumped_core \n") if ($verbose);
		$finished_processes{$running_processes{$waited_pid}} = [$exit_value,$signal_num,$dumped_core];
		delete($running_processes{$waited_pid});
	    }
	}
    }

    foreach my $i (keys(%finished_processes))
    {
	my $verdict = "BAD";
	if (($finished_processes{$i}->[0] == 0) && ($finished_processes{$i}->[1] == 0) && ($finished_processes{$i}->[2] == 0))
	{
	    $verdict = "GOOD";
	    $commandRuned{$nodes[$i]}=1;
	}
	else
	{
	    $commandRuned{$nodes[$i]}=0;
	    $ok=0;
	}
	print("$nodes[$i] : $verdict ($finished_processes{$i}->[0],$finished_processes{$i}->[1],$finished_processes{$i}->[2])\n") if ($verbose);
	
    }
    
    return $ok;
}



1;
