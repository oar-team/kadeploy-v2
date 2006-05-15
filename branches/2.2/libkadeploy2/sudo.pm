package sudo;

sub sudowrapping()
{
    
    my $kadeploy2_directory;
    my $file;
    my $command;
    my $pathfile;
    $kadeploy2_directory=libkadeploy2::conflib::get_conf("kadeploy2_directory");
    

    
    foreach $file (@listfiletowrapuser)
    {
	$pathfile="/usr/local/bin/$file";
	if ( ! -e $pathfile)
	{
	    $command="ln -s $kadeploy2_directory/bin/kasudowrapper.sh $pathfile";
	    print STDERR "Exec : $command\n";
	    system($command);
	}
	else
	{
	    print STDERR "$pathfile exist\n";
	}
    }

    foreach $file (@listfiletowraproot)
    {
	$pathfile="/usr/local/sbin/$file";
	if ( ! -e $pathfile)
	{
	    $command="ln -s $kadeploy2_directory/bin/kasudowrapper.sh $pathfile";
	    print STDERR "Exec : $command\n";
	    system($command);
	}
	else
	{
	    print STDERR "$pathfile exist\n";
	}
    }


    $command="$kadeploy2_directory/sbin/kasetup -exportenv";
    print "Exec : $command\n";
    system($command);
}



sub printvalidsudoers()
{
    my $kadeploydir=libkadeploy2::conflib::get_conf("kadeploy2_directory");
    my $kadeployuser=libkadeploy2::conflib::get_conf("deploy_user");
    my $tmpcmd;
    my $i;
    print "
Cmnd_Alias DEPLOYCMDUSER = ";
    for ($i=0; $i<=$#listfiletowrapuser; $i++)
    {
	print "$kadeploydir/bin/$listfiletowrapuser[$i]";
	if ($i!=$#listfiletowrapuser) { print ", "; }
    }    
    print ",$kadeploydir/sbin/setup_pxe.pl";
    print "\n";


    print "
Cmnd_Alias DEPLOYCMDROOT = ";
    for ($i=0; $i<=$#listfiletowraproot; $i++)
    {
	print "$kadeploydir/sbin/$listfiletowraproot[$i]";
	if ($i!=$#listfiletowraproot) { print ", "; }
    }    
    print "\n";

    print "
ALL ALL=($kadeployuser) NOPASSWD: DEPLOYCMDUSER
root ALL = (ALL) ALL
\n";

}

1;
