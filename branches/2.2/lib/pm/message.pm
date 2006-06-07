package libkadeploy2::message;
use strict;
use warnings;
use Sys::Syslog;

sub new()
{
    my $self;
    openlog("kadeploy", 'cons,pid', 'user');
    $self ={};
    bless $self;
    return $self;
}

sub severity($)
{
    my $self=shift;
    my $severity=shift;
    my $str;
    
    if ($severity<0)
    {
	$str="INFO    : ";
    }
    elsif ($severity==0)
    {
	$str="NOTICE  : ";
    }
    elsif($severity==1)
    {
	$str="WARNING : ";
    }
    elsif($severity==2)
    {
	$str="ERROR   : ";
    }
    elsif($severity>2)
    {
	$str="ERROR   : ";
    }

    return $str;
}

sub missing_node_cmdline($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="missing nodename";
    print STDERR $self->severity($severity).$msg."\n";
}

sub missing_login_cmdline($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="missing login";
    print STDERR $self->severity($severity).$msg."\n";
}

sub missing_envname_cmdline($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="missing envname";
    print STDERR $self->severity($severity).$msg."\n";
}


sub missing_rights_cmdline($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="missing rights";
    print STDERR $self->severity($severity).$msg."\n";
}

sub missing_flags_cmdline($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="missing flags";
    print STDERR $self->severity($severity).$msg."\n";
}

sub missing_cmdline($$)
{
    my $self=shift;
    my $severity=shift;
    my $cmdline=shift;
    my $msg="missing $cmdline";
    print STDERR $self->severity($severity).$msg."\n";

}

sub unknowerror($$)
{
    my $self=shift;
    my $severity=shift;
    my $specialmsg=shift;
    my $msg="Unknow error on ( $specialmsg )";
    print STDERR $self->severity($severity).$msg."\n";
    if ($severity>=0) { syslog('info', $msg); }
}


sub message($$)
{
    my $self=shift;
    my $severity=shift;
    my $msg=shift;
    print STDERR $self->severity($severity).$msg."\n";
    if ($severity>=0) { syslog('info', $msg); }
}

sub missing_node_db($)
{
    my $self=shift;
    my $severity=shift;
    my $nodename=shift;
    my $msg="node $nodename doesn't exist in db";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub missing_env_db($)
{
    my $self=shift;
    my $severity=shift;
    my $nodename=shift;
    my $msg="there is no environments in db";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}


sub notenough_right($$)
{
    my $self=shift;
    my $severity=shift;
    my $right=shift;
    my $msg="not enough rights => ".$right;
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub missing_rights_db($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="there is no rights in db";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}


sub missing_disk_db($)
{
    my $self=shift;
    my $severity=shift;
    my $nodename=shift;
    my $msg="Disk not found for node $nodename in db";
    print STDERR $self->severity($severity).$msg."\n";    

    if ($severity>=0) { syslog('info', $msg); }
}

sub osnotsupported($)
{
    my $self=shift;
    my $severity=shift;
    my $osname=shift;
    my $msg="$osname operating system is not supported yet";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }

}

sub dirnotfound($$)
{
    my $self=shift;
    my $severity=shift;
    my $dirname=shift;
    my $msg="the directory $dirname doesn't exist";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub filenotfound($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="the file $filename doesn't exist";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub erroropeningfile($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="can't open $filename";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub statfile($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="stating file $filename";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub loadingfile($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="loading $filename...";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub loadingfileDone($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="load file $filename finished.";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub loadingfilefailed($$)
{
    my $self=shift;
    my $severity=shift;
    my $filename=shift;
    my $msg="loading $filename failed";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub loadingfileyoumust($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="you have to load a file first";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub commandnodenamefailed($$$)
{
    my $self=shift;
    my $severity=shift;
    my $commandname=shift;
    my $nodename=shift;
    my $msg="$commandname failed for node $nodename";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}

sub checkingdb($)
{
    my $self=shift;
    my $severity=shift;
    my $msg="Checking database access...";
    print STDERR $self->severity($severity).$msg."\n";

    if ($severity>=0) { syslog('info', $msg); }
}


sub kanodes_help()
{
    my $self=shift;
    my $help="kanodes
\t--add                 add nodes
\t--del                 delete specified hostname

\t--listnode            list node from db
\t--listpartition       list partition from db 

\t-m|--machine          nodename

\t-h|--help             this help message
";

    print $help;
}

sub kamcat_help()
{
    my $self=shift;
    my $help="kamcat
\t-m|--machine          nodena

\t-p|--port             tcp port
\t--connector           rsh|ssh

\t-l|--login            username

\t--servercommand       \"local command\"
\t--clientcommand       \"distant command\"


\t-h|--help             this help message
";

    print $help;
}

sub kaexec_help()
{
    my $self=shift;
    my $help="kaexec
\t--confcommand         execute a configuration command on a set of node
\t--nodecommand         execute a command on a set of node

\t-m|--machine          node name
\t-f                    node file

\t-c|--command          command to exec on a set of node

\t--connector           rsh|ssh
\t-l|--login            username
\t-t|--timeout          timeout for command

\t-v|--verbose          verbose mode
\t-h|--help             this help message
";

    print $help;
}


sub kaenv_help()
{
    my $self=shift;
    my $help="kaenv
\t--add                 add environment to db
\t--del                 delete environemnt to db
\t--list                list environment

\t-e|--environment      environment

\t-l|--login            username

\t-f|--envfile          environment description file

\t-h|--help             this help message
";

    print $help;
}




sub kaconsole_help()
{
    my $self=shift;
    my $help="kaconsole
\t-m|--machine          nodename

\t-h|--help             this help message
";

    print $help;
}

sub kachecknodes_help()
{
    my $self=shift;
    my $help="kachecknodes
\t--check               whant to check some nodes
\t--list                show result from a check

\t--type                choose a check type [ICMP|SSH|MCAT]
\t--retry               blocking time in 's'

\t-m|--machine          nodename
\t-v|--verbose          be verbose
\t-h|--help             this help message
";

    print $help;
}



sub kareboot_help()
{
    my $self=shift;
    my $help="kareboot
\t-m|--machine          nodename
\t-f|--nodefile         nodefile

\t-e|--environment      environment_name
\t-p|--part-number      partition number
\t-d|--disk-number      disk number

\t--soft                softboot
\t--hard                hardboot
\t--deploy              deploy

\t-v|--verbose          verbose on
\t-h|--help             this help message
";

    print $help;
}


sub karights_help()
{
    my $self=shift;
    my $help="karights
\t--add                 add a right 
\t--del                 remove a right
\t--list                list
\t-m|--machine          nodename
\t-f|--nodefile         nodefile

\t-r|--rights           rights
\t-l|--login            username
\t-h|--help             this help message
";

    print $help;
}

sub kapxe_help()
{
    my $self=shift;
    my $help="kapxe
\t-m|--machine          nodename

\t--type [type]         pxe boot loader [pxelinux|grub|windows]

\t-k|--kernel           path to kernel
\t-i|--initrd           path to initrd
\t--kernelparams        kernel parameters

\t--serialport          serialport number
\t--serialspeed         serialport speed
\t--timeout             pxe loader timeout

\t-h|--help             this help message
";

    print $help;
}

sub kadeploy_help()
{
    my $self=shift;
    my $help="kadeploy
\t-m|--machine          nodename
\t-f|--nodefile         nodefile

\t-p|--part-number      partition number
\t-d|--disk-number      disk number

\t-e|--environment      environment name
\t-l|--login            username

\t-h|--help             this help message
";
    print $help;
}

sub deployenv_help()
{
    my $self=shift;
    my $help="deployenv
\t-m|--machine          nodename
\t-f|--nodefile         nodefile

\t-p|--part-number      partition number
\t-d|--disk-number      disk number

\t-e|--environment      environment name
\t-l|--login            username

\t-h|--help             this help message
";
    print $help;
}




sub kapart_help()
{
    my $self=shift;
    my $help="kapart
\t-m|--machine          nodename
\t-f|--nodefile         nodefile


\t-d|--disk-number      disk number
\t   --disktype         disk type [ide|scsi|sata]

\t  --ostype            linux currently


\t-v|--verbose          verbose on
\t-h|--help             this help message
";
    print $help;
}


1;
