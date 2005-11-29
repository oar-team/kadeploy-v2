#!/usr/bin/perl

#
# mcat.pl <mcat_path> <src_cmd> <node_cmd> <file of node (first one is src)>
#
# ex1: ./mcat.pl /home/grenoble/orichard/Prog/Mcat/mcat 12100 "cat /tmp/toto" "cat > /tmp/titi" $OAR_NODEFILE
# ex2: ./mcat.pl /home/grenoble/orichard/Prog/Mcat/mcat 12100 "cat  ~/yop.tgz" "tar -zxv -C /tmp/" $OAR_NODEFILE

use strict;
use Data::Dumper;

my $sentinelle_path = "/usr/local/bin/sentinelle.pl";
my $args_sentinelle = "-crsh -lroot -ctimeout=2000 -v ";

sub hostname2ip {
	my $hostname = shift;
        print "hostname $hostname";
	# Déclaration des variables utilisées :
	my (@bytes,$name,$altnames,$addrtype,$len,$packaddr);
	my ($ip);
	(my $name, my $altnames, my $addrtype, my $len, my @addrlist) = gethostbyname ($hostname);
		
	$ip=join('.',(unpack("C4",$addrlist[0])));
	return ($ip);
}


my $node;
my $nodes;
my $sentinelle_nodes;

my $port = $ARGV[1];
my $mcat_path = $ARGV[0];
my $cmd_scr = $ARGV[2];
my $cmd     = $ARGV[3];
my $file = $ARGV[4];
my $name;
my $altnames;
my $addrtype;
my $len;
my @addrlist;
my $ip;
my $yop;
#my $ip=join('.',(unpack("C4",$addrlist[0])));
#print "Pas glop :",$ip,"\n";

open(FILE,"<$file") or die("File trouble");
while (<FILE>) {
        chomp;
#        print "Node $_ \n";
        $node = $_;
        
        ($name, $altnames, $addrtype, $len, @addrlist) = gethostbyname($node);
        $ip=join('.',(unpack("C4",$addrlist[0])));

#                print "IP $ip\n"; 

        $nodes = $nodes." ".$ip;
	$sentinelle_nodes = $sentinelle_nodes." -m ".$ip;
        print "Node list :",$nodes, "\n";
}

my $mputcat = $sentinelle_path." ".$args_sentinelle." ".$sentinelle_nodes." -- ".$mcat_path." ".$port." \"\\\"".$cmd_scr."\"\\\" \"\\\"".$cmd."\"\\\" ".$nodes;
print "Commande : $mputcat \n";
print "Gazzz...\n";
system($mputcat);
