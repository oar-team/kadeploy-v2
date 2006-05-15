package tftppxe;

sub createtftp()
{
    my $kadeployuser;
    my $kadeploydir; 
    my $tftpdir;
    my $pxedir;
    my $tftpbootdir;
    my $pxelinux;
    my $memdisk;
    my $deployx86;
    my $deployx8664;

    $kadeployuser=libkadeploy2::conflib::get_conf("deploy_user");
    $kadeploydir=libkadeploy2::conflib::get_conf("kadeploy2_directory");
    $tftpdir=libkadeploy2::conflib::get_conf("tftp_repository");
    $pxedir=$tftpdir."/".libkadeploy2::conflib::get_conf("pxe_rep");
    $tftpbootdir=$tftpdir."/".libkadeploy2::conflib::get_conf("tftp_relative_path");
    $pxelinux="$kadeploydir/lib/pxelinux/pxelinux.0";
    $memdisk="$kadeploydir/lib/pxelinux/memdisk";
    $deployx86="$kadeploydir/lib/deployment_kernel/x86/";
    $deployx8664="$kadeploydir/lib/deployment_kernel/x86_64/";


    system("mkdir -p $tftpdir");
    system("mkdir -p $pxedir");
    system("mkdir -p $tftpbootdir");

    system("cp $pxelinux $tftpdir");
    system("cp $memdisk  $tftpbootdir");
    system("cp $deployx86/*   $tftpbootdir");
    system("cp $deployx8664/* $tftpbootdir");
    system("chown -R $kadeployuser $tftpdir");
    print("Done !!!!\n");
}

1;
