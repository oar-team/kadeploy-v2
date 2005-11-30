#!/usr/bin/perl

#
# mcat.pl -cp <mcat_path_srv> -sc <src_cmd> -dc <node_cmd> -m <node1> -m <node2>
#
#./mcat_rsh.pl -p 12000 -m 172.24.1.8 -cp /home/grenoble/jleduc/Mcat/mcatseg -sc "cat /home/grenoble/jleduc/debian_grid5000_ldap.tgz" -dc "cat > /pre_pipe"
#
#pour lancer une commande a distance, il faut generer:
#/home/grenoble/jleduc/unstable/kadeploy2/tools/sentinelle/sentinelle.pl -c ssh -t 100 -v -w 2 -m 172.24.6.32 -p "\"/home/grenoble/jleduc/unstable/kadeploy2/tools/Mcat/mcatseg 1 12002 \\\"/bin/cat /home/grenoble/jleduc/Images/image_debian_min_sun_ibm.tgz\\\" \\\"/bin/cat > /tmp/test\\\" 172.24.6.32\""
#
#
#
#exemple d'utilisation qui marche bien:
#
#./mcat_test.pl -p 12001 -m 172.24.3.34 -m 172.24.6.33 -m 172.24.11.23 -cp /home/grenoble/jleduc/unstable/kadeploy2/tools/Mcat/mcatseg -sc "cat /home/grenoble/jleduc/Images/image_debian_min_sun_ibm.tgz" -dc "cat > /tmp/test2"


use Getopt::Long;
use strict;
use Data::Dumper;

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



my $sentinelle_path = "/home/grenoble/jleduc/unstable/kadeploy2/tools/sentinelle/sentinelle.pl";
my $args_sentinelle = "-c ssh -t 100 -v ";

my $port = $port[0];
my $mcat_path = $cmd_path[0];
my $cmd_scr = $src_cmd[0];
my $cmd     = $dest_cmd[0];
my $nb_nodes = scalar(@host_list) + 1;

$args_sentinelle .= "-w " . $nb_nodes;

my $sentinelle_nodes = "-m " . join(' -m ', @host_list);
my $nodes = join(" ", @host_list);


# for the nodes
my $mputcat_nodes = $sentinelle_path." ".$args_sentinelle." ".$sentinelle_nodes." -p \"\\\"$mcat_path 1 $port \\\\\\\"".$cmd_scr."\\\\\\\" \\\\\\\"".$cmd."\\\\\\\" ".$nodes . "\\\"\"";
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
