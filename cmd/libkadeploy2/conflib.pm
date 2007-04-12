###############################################################################
##  *** ConfLib: *** 
##
## Description: module de gestion du fichier de conf de DEPLOY
##
## Une ligne du fichier de conf est de la forme:
##  > truc = 45 machin chose bidule 23 # un commentaire
##
## Vous pouvez commencer des lignes de commentaires par "#", elles seront
## ignorees de meme d'ailleurs que toutes les lignes non conformes a
## l'expression reguliere definissant une ligne valide...:)
##
## Apres initialisation du modules a l'aide de la fonction check_conf(),
## la recuperation d'un parametre se fait avec la fonction get_conf("truc").
## La fonction is_conf quant � elle permet de savoir si un parametre est defini.
##
## - Exemple d'utilisation:
##  > use ConfLib qw(init_conf get_conf is_conf);
##  > init_conf();
##  > print "toto = ".get_conf("toto")."\n" if is_conf("toto");
##
###############################################################################
package libkadeploy2::conflib;

use strict;
use warnings;
require Exporter;

use libkadeploy2::deploy_iolib;

#our (@ISA,@EXPORT,@EXPORT_OK);
#@ISA = qw(Exporter);
#@EXPORT_OK = qw(init_conf get_conf is_conf dump_conf reset_conf);

## prototypes
sub check_conf;
sub check_cmd;
sub check_nodes_conf;
sub check_cmd_exist;
sub check_db_access;
sub get_conf;
sub is_conf;
sub dump_conf;

## parameters container
#my %params;

## regex pour une ligne valide du fichier de conf.
my $regex = qr{^\s*([^#=\s]+)\s*=\s*([^#]*)};

my $default_deployconf = "/etc/kadeploy/deploy.conf";
my $default_deploycmdconf = "/etc/kadeploy/deploy_cmd.conf";



sub new {
    my ($class) = @_;
    my $self = {};
    $self->{deployconf} = $default_deployconf;
    $self->{deploycmdconf} = $default_deploycmdconf;
    $self->{params} = ();
print "toto" . $self->{deploycmdconf} . "\n";
    bless ($self, $class);
    return $self;
}



## check_conf
## checks the configuration file
## parameters : /
## return value : 1 if conf file actually loaded, else 0.
sub check_conf {
    my $config = shift;
    my $deployconf = $config->{deployconf};

    if ($deployconf eq "") {
	print "ERROR: kadeploy configuration file not defined\n";
	return 0;
    }

    my %critic = (
		  #######                   legende                    ########
                  #   nb  type de data                   - action realisee
		  # -----------------------------------------------------------
                  #   1 = nombre ou options ou simple chaine - 1 pas de check
                  #   2 = /path/                             - 2 check / debut & / fin
		  #   3 = /path/cmd ou /path/archive.tgz     - 3 check / debut & pas / fin
		  #   4                                      - 4 check pas / debut mais / fin
		  #   5                                      - 5 check pas de / ni debut ni fin
		  #   6                                      - 6 check / debut & peu importe fin
		  #"remote_sentinelle_rsh" => 3,
		  #"remote_sentinelle_rsh_default_args" => 1,
		  #"remote_mcat" => 3,
		  # ce ne sont pas des variables critiques
		  #"use_internal_parallel_command" => 7,
		  #"do_fdisk_on_deploy" => 7,
		  
		  "kadeploy2_directory" => 6,
		  "first_check_timeout" => 1,
		  "last_check_timeout" => 1,
		  "enable_nmap" => 1,
		  "nmap_cmd" => 3,

		  "deploy_rcmd" => 1,
		  "prod_rcmd" => 1,

		  "deploy_db_host" => 1,
		  "deploy_db_name" => 1,
		  "deploy_db_login" => 1,
		  "deploy_db_psswd" => 1,

		  "pre_install_archive" => 3,
		  "pre_install_script" => 5,
		  "post_install_script" => 5,
		  "tftp_repository" => 2,
		  "pxe_rep" => 4,
		  "tftp_relative_path" => 5,
		  #############################
		  );

    my %already_defined = ();

    my $twice = 0;
    my $undefined = 0;
    my $missing = 0;

    if(!(-e $deployconf)){
	print "ERROR : variable configuration file: " . $deployconf . " does not exist\n";
	return 0;
    }

    print STDERR "Checking variable definition...\n";
    open(DEPLOYCONF,$deployconf) or die "Can't open $deployconf, maybe you are not allowed to open this file\n";

    foreach my $line (<DEPLOYCONF>){
	if ($line =~ $regex) {
	    my ($key,$val) = ($1,$2);
	    $val =~ s/\s*$//;
	    if(!exists($config->{params}{$key})){
                $config->{params}{$key} = $val;
            }else{
                print "ERROR : variable $key is defined twice \n";
                $twice = 1;
            }
	}
    }
    close(DEPLOYCONF);

    # checks if the critic variables are defined 
    foreach my $var (keys %critic){
	if(!exists($config->{params}{$var})){
	    print "ERROR : critic variable $var is missing\n";
	    $missing = 1;
	}else{# critic variable is defined

	    my $type = $critic{$var};
	    my $valid = 0;
	    if ($type == 2) { # check / debut & fin
		if (!($config->{params}{$var} =~ /^\/.*\/$/)){
		    print "ERROR : $var variable should start and end with an / \n";
		    $missing = 1;
		}
	    }elsif($type == 3){ # check / debut & pas / fin
		if ((!($config->{params}{$var} =~ /^\/.*/)) || ($config->{params}{$var} =~ /.*\/$/)){
		    print "ERROR : $var variable should start with an / and end without\n";
		    $missing = 1;
		}
	    }elsif($type == 4){ # check / fin & pas / debut
		if (($config->{params}{$var} =~ /^\/.*/) || (!($config->{params}{$var} =~ /.*\/$/))){
		    print "ERROR : $var variable should end with an / and start without\n";
		    $missing = 1;
		}
	    }elsif($type ==5){ # check pas de / ni debut ni fin
		if (($config->{params}{$var} =~ /^\/.*/) || ($config->{params}{$var} =~ /.*\/$/)){
		    print "ERROR : $var variable should not start neither end with an / \n";
		    $missing = 1;
		}
	    }elsif($type == 6){ # check / debut & peu importe fin
		if (!($config->{params}{$var} =~ /^\/.*/)){
		    print "ERROR : $var variable should start with an /\n";
		    $missing = 1;
		}
	    }elsif($type == 7){
		if (!(
		      ($config->{params}{$var} =~ /yes/) ||
		      ($config->{params}{$var} =~ /no/)
		      )
		    )
		{
		    print "ERROR :$var should be yes or no\n";
		    $missing = 1;
		}		    	
	    }else{ # pas de check
		;
	    }
	}
    }

    if ($twice || $missing){
	print "ERROR : please check your configuration file\n";
	return 0;
    }

    return 1;
}

## check_cmd
## checks the command configuration file
## parameters : /
## return value : hash of hash as follow hostname => cmd_type => cmd 
sub check_cmd {
    my $config = shift;
    my $undefined = 0;
    my %res;

    if(!(-e $config->{deploycmdconf})){
	print "ERROR : command configuration file does not exist\n";
	return 0;
    }

    print "Checking command definition...\n";
    open(DEPLOYCMD, $config->{deploycmdconf});
    
    foreach my $line (<DEPLOYCMD>){
	my $cmd = "";
	chomp($line);
	# checks if the line exists
	if($line){
	    # checks if it is a commentary
	    if($line !~ /^\s*#.*/){
	       # parses line info
	       my @info = split(/\s+/, $line);
	       my $size = @info;
	       for(my $i = 2; $i < $size; $i++){
		   #print "CMD = $cmd ; info[i] = $info[$i]\n";
		   $cmd = $cmd.$info[$i];
	       }
	       $res{$info[0]}{$info[1]} = $cmd;
	   }
	}
    }
    close(DEPLOYCMD);

    return %res;
}


sub check_nodes_conf {
    my $config = shift;
    my $nodes_list_ref = shift;
    my @nodes_list = @{$nodes_list_ref};
    my $main_conf_file = "";

    my %res = check_cmd();

    my $loop_conf_file; 
    # retrieve main configuration file name
    foreach my $node (@nodes_list) {
        if (!exists($res{$node}{"configuration"})) {
                $loop_conf_file = $default_deployconf;
        } else {
                $loop_conf_file = $res{$node}{"configuration"};
        }
	if (($main_conf_file ne "") && ($loop_conf_file ne $main_conf_file )) {
                print "ERROR: all the node are not from the same cluster, please check again the specified nodes\n";
                return 0;
        }
        $main_conf_file = $loop_conf_file;
    }

    $config->{deployconf} = $main_conf_file;

    return 1;
}




## check_cmd_exist
## checks if the command configuration file exists
## parameters : /
## return value : 1 if conf file actually loaded, else 0.
sub check_cmd_exist {
    my $config = shift;
    
    if (!-r $config->{deploycmdconf} ) {
	print "ERROR : command configuration file does not exist\n";
	exit 0;
    }
    
    return 1;
}


## check_db_access
## tries to connect to databases 
## parameters : /
## return value : 1 if ok
sub check_db_access {
    return 1;
    print "Checking database access rights...\n";
    my $base = libkadeploy2::deploy_iolib::connect();
    libkadeploy2::deploy_iolib::disconnect($base);
    
    return 1;
}    

# recupere un parametre
sub get_conf {
  my $config = shift;
  my $key = shift;
  (defined $key) or print "WARNING : get_conf expects a parameter \n";
  return $config->{params}{$key};
}

# teste si un parametre est defini
sub is_conf {
  my $config = shift;
  my $key = shift;
  (defined $key) or print "WARNING : is_conf expects a parameter\n";
  return exists $config->{params}{$key};
}

# debug: dump les parametres
sub dump_conf {
  my $config = shift;
  print "Config file is: ".$config->{deployconf}."\n";
  while (my ($key,$val) = each %{$config->{params}}) {
    print " ".$key." = ".$val."\n";
  }
  return 1;
}

return 1;
