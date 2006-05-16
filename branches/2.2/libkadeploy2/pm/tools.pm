package libkadeploy2::tools;

use libkadeploy2::deploy_iolib;



sub returnvalidhostsfile()
{
    my $db=libkadeploy2::deploy_iolib::new();
    $db->connect();
    my $refnodehash;
    my %nodehash;
    my $key;
    my $strret;
    $refnodehash=$db->get_node();
    %nodehash=%$refnodehash;
    $strret="127.0.0.1 localhost\n";
    foreach $key (keys(%nodehash))
    {
	$strret.="$nodehash{$key} $key\n";
    }
    $db->disconnect();
    return $strret;
}


1;
