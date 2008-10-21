package libkadeploy2::confroot;

use libkadeploy2::debug;
use Env qw(KADEPLOY_CONFIG_DIR);

use strict;
use warnings;

my $default_config_dir="/etc/kadeploy";
my $kadeploy_config_dir="";
my $kaenv='KADEPLOY_CONFIG_DIR';

sub get_conf_rootdir()
{
  if ( defined($ENV{$kaenv}) ) {
    $kadeploy_config_dir = $ENV{$kaenv}
  } else {
    if ($kadeploy_config_dir eq "") {
      $kadeploy_config_dir = $default_config_dir;
    }
  }
  return $kadeploy_config_dir;
}

sub set_conf_rootdir($)
{
  my $rootdir = shift;
  
  $ENV{$kaenv} = $rootdir;
  $kadeploy_config_dir = $rootdir;
}

sub info()
{
  print "[I] Configuration used : $kadeploy_config_dir\n";
}

1;

