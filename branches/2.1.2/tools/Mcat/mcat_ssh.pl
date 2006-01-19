#!/usr/bin/perl

#
# mcat.pl -cp <mcat_path_srv> -sc <src_cmd> -dc <node_cmd> -m <node1> -m <node2>
#
#./mcat_rsh.pl -p 12000 -m 172.24.1.8 -cp /home/grenoble/jleduc/Mcat/mcatseg -sc "cat /home/grenoble/jleduc/debian_grid5000_ldap.tgz" -dc "cat > /pre_pipe"
#

use Getopt::Long;
use strict;
use Data::Dumper;
use libkadeploy2::conflib;

# useless now
#my $server_addr = "172.24.100.2";

my @cmd_path;
my @port;
my @src_cmd;
my @dest_cmd;
my @host_list;


## gets the options
GetOptions('p=s'             => \@port,
           'cp=s'            => \@cmd_path,
	   'sc=s'            => \@src_cmd,
	   'dc=s'            => \@dest_cmd,
           'm=s'           => \@host_list,
           'machine=s'     => \@host_list,
	   );



#my $sentinelle_path = "/usr/bin/sentinelle";
#my $args_sentinelle = "-cssh -ctimeout=2000 -v ";

my $sentinelle_path = libkadeploy2::conflib::get_conf("prod_sentinelle_cmd");
my $args_sentinelle = libkadeploy2::conflib::get_conf("prod_sentinelle_default_args");


my $port = $port[0];
my $mcat_path = $cmd_path[0];
my $cmd_scr = $src_cmd[0];
my $cmd     = $dest_cmd[0];

my $sentinelle_nodes = "-m " . join(' -m ', @host_list);
my $nodes = join(" ", @host_list);


# for the nodes
my $mputcat_nodes = $sentinelle_path." ".$args_sentinelle." ".$sentinelle_nodes." -- $mcat_path 1 ".$port." \"\\\"".$cmd_scr."\"\\\" \"\\\"".$cmd."\"\\\" ".$nodes;
my $mputcat_src = $mcat_path . " 4 " . $port . "  \"" . $cmd_scr . "\" \"\\\"" .$cmd."\"\\\" " . $host_list[0];  

#my $mputcat = $sentinelle_path." ".$args_sentinelle." ".$sentinelle_nodes." -- ".$mcat_path." ".$port." \"\\\"".$cmd_scr."\"\\\" \"\\\"".$cmd."\"\\\" ".$nodes;
print "Commande noeuds : $mputcat_nodes \n";
print "Commande serveur : $mputcat_src \n";
print "Gazzz...\n";
#system($mputcat);

my $child_pid;
if (!defined($child_pid = fork())) {
	die "cannot fork: $!";
	} elsif ($child_pid) {
	print "Forking child $child_pid\n";
	} else {
	print "child\n";
	system("$mputcat_nodes");
	exit;
	}
print "parent\n";	
system($mputcat_src);
print "src command finished\n";
exit;
