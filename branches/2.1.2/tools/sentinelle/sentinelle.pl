#!/usr/bin/perl
use IPC::Open3;
use Getopt::Long;
use Data::Dumper;
use strict;
use warnings;


sub usage(){
    print <<EOU;
Usage sentinelle.pl [-h | -m node][-c connector][-w window_size][-t timeout][-p program][-v]
    -h display this help message
    -m specify the node to contact (use several -m options for several nodes)
    -c connector to use (default is ssh)
    -l user to use (default is script launcher)
    -w window size (number of fork at the same time)
    -t timeout for each command in second
    -p programm to run (default is "true")
    -v verbose mode
EOU
                                    
}

my @nodes;
my $command = "true";
my $window_size = 5;
my $connector = "ssh";
my $user = "";
my $timeout = 30;
my $verbose;
my $sos;

Getopt::Long::Configure ("gnu_getopt");
GetOptions ("machine|m=s" => \@nodes,
            "program|p=s" => \$command,
            "connector|c=s" => \$connector,
	    "user|l=s"  => \$user,
            "timeout|t=i" => \$timeout,
            "window|w=i" => \$window_size,
            "verbose|v" => \$verbose,
            "help|h" => \$sos
           );

if (defined($sos)){
    usage();
    exit(0);
}

select STDOUT;
$| = 1;

my $nbNodes = $#nodes + 1;
my $index = 0;
my %running_processes;
my %finished_processes;

while (scalar(keys(%finished_processes)) <= $#nodes){
    while((scalar(keys(%running_processes)) < $window_size) && ($index <= $#nodes)){
        print("[VERBOSE] fork process for the node $nodes[$index]\n") if ($verbose);
        my $pid = fork();
        if (defined($pid)){
            if ($pid == 0){
                #In the child
                # Initiate timeout
                alarm($timeout);
		my $cmd;
		if ( $user ) {
                	$cmd = "$connector -l $user $nodes[$index] $command";
		} else {
			$cmd = "$connector $nodes[$index] $command";
		}
                print("[VERBOSE] Execute command : $cmd\n") if ($verbose);
                exec($cmd);
            }
            $running_processes{$pid} = $index;
        }else{
            warn("/!\\ fork system call failed for node $nodes[$index].\n");
        }
        $index++;
    }
    my $pid = wait();
    my $exit_value = $? >> 8;
    my $signal_num  = $? & 127;
    my $dumped_core = $? & 128;

    if ($pid == -1){
        die("/!\\ wait return -1 so there is no child process. It is a mistake\n");
    }else{
        if (defined($running_processes{$pid})){
            print("[VERBOSE] Child process $pid ended : exit_value = $exit_value, signal_num = $signal_num, dumped_core = $dumped_core \n") if ($verbose);
            $finished_processes{$running_processes{$pid}} = [$exit_value,$signal_num,$dumped_core];
            delete($running_processes{$pid});
        }
    }
}

foreach my $i (keys(%finished_processes)){
    my $verdict = "BAD";
    if (($finished_processes{$i}->[0] == 0) && ($finished_processes{$i}->[1] == 0) && ($finished_processes{$i}->[2] == 0)){
        $verdict = "GOOD";
    }
    print("$nodes[$i] : $verdict ($finished_processes{$i}->[0],$finished_processes{$i}->[1],$finished_processes{$i}->[2])\n");
}

