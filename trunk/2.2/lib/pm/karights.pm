package libkadeploy2::karights;
use strict;
use warnings;
use libkadeploy2::deployconf;
use libkadeploy2::deploy_iolib;

sub check_db($$$)
{
    my $user=shift;
    my $node=shift;
    my $right=shift;

    my $i;
    my $rights;
    my $db;
    my $ok=1;

    $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    if (! $db->check_rights($user,$node->get_name(),$right)) { $ok=0; }
    $db->disconnect();
    return $ok;
}

sub check_rights($$)
{
    my $nodelist=shift;
    my $right=shift;

    my $ref_node_list;
    my @node_list;
    my $ok=0;
    my $count=0;
    my $listok=0;
    my $sudo_user;
    my $node;
    my $conf=libkadeploy2::deployconf::new();
    
    
    $conf->load();

    $sudo_user=libkadeploy2::sudo::get_sudo_user();
    if (! $sudo_user) { $sudo_user=libkadeploy2::sudo::get_user(); }
    if ( $sudo_user eq "root" ||
	 $sudo_user eq $conf->get("deploy_user") ||
	 $sudo_user eq $conf->get("privileged_user")
	 )
    {
	$ok=1;
    }
    
    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    foreach $node (@node_list)
    {
	$count++;
	if (libkadeploy2::karights::check_db($sudo_user,
					     $node,
					     $right))
	{
	    $listok++;
	}
    }    
    if (! $ok) { if ($count==$listok && $count!=0) { $ok=1; } }
    return $ok;
}



1;
