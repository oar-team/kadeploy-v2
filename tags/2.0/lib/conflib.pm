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
package conflib;

use strict;
use warnings;
require Exporter;

use lib::deploy_iolib;

our (@ISA,@EXPORT,@EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_conf get_conf is_conf dump_conf reset_conf);

## prototypes
sub check_conf;
sub check_cmd;
sub check_cmd_exist;
sub check_db_access;
sub get_conf($);
sub is_conf($);
sub dump_conf();
sub reset_conf();

## configuration  file
my $file = undef;
## parameters container
my %params;
## regex pour une ligne valide du fichier de conf.
my $regex = qr{^\s*([^#=\s]+)\s*=\s*([^#]*)};

if(!(check_conf() == 1)){
    print "ERROR : configuration file loading failed\n";
    exit 0;
}

## check_conf
## checks the configuration file
## parameters : /
## return value : 1 if conf file actually loaded, else 0.
sub check_conf {
    my %critic = (
		  "first_check_timeout" => 0,
		  "last_check_timeout" => 0,
		  "write_rate" => 0,
		  "enable_nmap" => 0,
		  "deploy_sentinelle_cmd" => 0,
		  "deploy_sentinelle_default_args" => 0,
		  "deploy_sentinelle_pipelined_args" => 0,
		  "deploy_sentinelle_endings" => 0,
		  "deploy_sentinelle_timeout" => 0,
		  "deploy_db_host" => 0,
		  "deploy_db_name" => 0,
		  "deploy_db_login" => 0,
		  "deploy_db_psswd" => 0,
		  "pre_install_archive" => 0,
		  "pre_install_script" => 0,
		  "post_install_script" => 0,
		  "kadeploy2_directory" => 0,
		  );

    my %already_defined = ();

    my $twice = 0;
    my $undefined = 0;
    my $missing = 0;

    if(!(-e "/etc/kadeploy/deploy.conf")){
	print "ERROR : variable configuration file does not exist\n";
	return 0;
    }

    print "Checking variable definition...\n";
    open(DEPLOYCONF,"/etc/kadeploy/deploy.conf");
    %params = ();

    foreach my $line (<DEPLOYCONF>){
	if ($line =~ $regex) {
	    my ($key,$val) = ($1,$2);
	    $val =~ s/\s*$//;
	    if(!exists($params{$key})){
                $params{$key}=$val;
            }else{
                print "ERROR : variable $key is defined twice \n";
                $twice = 1;
            }
	}
    }
    close(DEPLOYCONF);

    # checks if the critic variables are defined 
    foreach my $var (keys %critic){
	if(!exists($params{$var})){
	    print "ERROR : critic variable $var is missing\n";
	    $missing = 1;
	}
    }

    if ($twice || $missing){
	print "ERROR : please check your configuration file\n";
	return 0;
    }

    #checks if the values of the critic variables are correct (when possible) ?
    #print "Checking variables values...\n";

    return 1;
}

## check_cmd
## checks the command configuration file
## parameters : /
## return value : hash of hash as follow hostname => cmd_type => cmd 
sub check_cmd{
    my $undefined = 0;
    my %res;

    if(!(-e "/etc/kadeploy/deploy_cmd.conf")){
	print "ERROR : command configuration file does not exist\n";
	return 0;
    }

    print "Checking command definition...\n";
    open(DEPLOYCMD,"/etc/kadeploy/deploy_cmd.conf");
    
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
		   $cmd = $cmd.$info[$i]." ";
	       }
	       $res{$info[0]}{$info[1]} = $cmd;
	   }
	}
    }
    close(DEPLOYCMD);
    
    return %res;
}

## check_cmd_exist
## checks if the command configuration file exists
## parameters : /
## return value : 1 if conf file actually loaded, else 0.
sub check_cmd_exist {
    my $undefined = 0;
    
    if ( -r "/etc/kadeploy/deploy_cmd.conf" ) {
	$file = "/etc/kadeploy/deploy_cmd.conf";
    } else {
	print "ERROR : command configuration file does not exist\n";
	exit 0;
    }
    
    return 1;
}

## check_db_access
## tries to connect to databases 
## parameters : /
## return value : 1 if ok
sub check_db_access{
    print "Checking database access rights...\n";
    my $base = deploy_iolib::connect();
    deploy_iolib::disconnect($base);
    
    return 1;
}    

# recupere un parametre
sub get_conf ( $ ) {
  my $key = shift;
  (defined $key) or print "WARNING : get_conf expects a parameter \n";
  return $params{$key};
}

# teste si un parametre est defini
sub is_conf ( $ ) {
  my $key = shift;
  (defined $key) or print "WARNING : is_conf expects a parameter\n";
  return exists $params{$key};
}

# debug: dump les parametres
sub dump_conf () {
  print "Config file is: ".$file."\n";
  while (my ($key,$val) = each %params) {
    print " ".$key." = ".$val."\n";
  }
  return 1;
}

# reset the module state
sub reset_conf () {
  $file = undef;
  %params = ();
  return 1;
}

return 1;