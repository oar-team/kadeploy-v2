package libkadeploy2::kaenv;

use strict;
use warnings;

use Getopt::Long;
use libkadeploy2::conflib;
use libkadeploy2::deploy_iolib;
use libkadeploy2::hexlib;
use libkadeploy2::message;
use libkadeploy2::errorhandler;
use libkadeploy2::environment;
use libkadeploy2::environments;
use libkadeploy2::sudo;

sub listenv();
sub check_options();
sub check_rights();

my $conf=libkadeploy2::deployconf::new();
if (! $conf->loadcheck()) { exit 1; }
my $errorhandler=libkadeploy2::errorhandler::new();
my $message=libkadeploy2::message::new();
my $sudo_user=libkadeploy2::sudo::get_sudo_user();



my $ok=1;

if (! $sudo_user) { $sudo_user=libkadeploy2::sudo::get_user(); }

sub new()
{
    my $self=
    {
	add => 0,
	del => 0,
	list => 0,
	help => 0,
	showenv => 0,
	
	envfile => "",
	login   => "",
	envname => "",
    };
    bless $self;
    return $self
}

sub run()
{
    my $self=shift;
    my $subname="run";
    my $retcode=0;
    my $ok=1;
    
    if (! $self->check_options())
    { $errorhandler->program_exit("kaenv::run","checking option..."); }

    if ($self->{help}) { $message->kaenv_help(); return 0; }
    
    if ($self->{add})
    {
	$ok=$self->add();
    }
    elsif ($self->{del})
    {
	$ok=$self->del();
    }    
    elsif ($self->{list})
    {
	$ok=$self->listenv();
    }
    elsif ($self->{showenv})
    {
	$ok=$self->showenv();
    }
    else
    {
	$message->missing_cmdline(2,"flags");
	$ok=0;
    }

    if ($ok) { $retcode=0; } else { $retcode=1; }
    
    return $retcode;	
}

sub add()
{
    my $self=shift;
    my $ok=0;
    my $subname="kaenv::add";
    if ($self->{envfile} && $self->{login} && $self->{envname})
    {
	my $env=libkadeploy2::environment::new();
	$ok=1;
	$env->set_name($self->{envname});
	$env->set_user($self->{login});
	$env->set_descriptionfile($self->{envfile});
	if ($self->{envfile} =~ /^\//)
	{
	    if (! $env->addtodb()) { $ok=0; }
	    if ($ok)   { $message->message(0,"add environment ".$self->{envname}." to db"); }
	    if (! $ok) { $errorhandler->error_in($subname,"Fail to add environment ".$self->{envname}." to db"); }
	}
	else
	{
	    $message->message(2,"you must specify an absolute path");
	    $ok=0;
	}
    }
    return $ok;
}

sub del()
{
    my $self=shift;
    my $ok=1;
    my $subname="kaenv::del";
    if ($self->{envfile} && $self->{login} && $self->{envname})
    {
	$ok=1;
	my $env=libkadeploy2::environment::new();
	$env->set_name($self->{envname});
	$env->set_user($self->{login});
	$env->set_descriptionfile($self->{envfile});
	if (! $env->delfromdb()) { $ok=0; }
	if ($ok)   { $message->message(0,"del environment ".$self->{envname}." from db"); }
	if (! $ok) { $errorhandler->error_in($subname,"Fail to del environment ".$self->{envname}." from db"); }
	$ok=1;
    }
    else
    { $ok=0; }
    return $ok;	
}

sub check_options()
{
    my $self=shift;
    my $subname="kaenv::check_options";
    my $ok=1;

    if ($self->{help})                      { $message->kaenv_help(); $errorhandler->program_exit(); }
    if ($self->{add} || $self->{del}) 
    {
	if (! $self->{login})               { $errorhandler->error_in($subname,"user name needed"); }
	if (! $self->{envname})             { $errorhandler->error_in($subname,"environment name needed"); }
    }
    if (! $self->check_rights()) 
    { $errorhandler->error_in($subname,"$sudo_user not allowed to kaenv"); }

    if (! $self->{add} &&
	! $self->{del} &&
	! $self->{list} &&
	! $self->{help})
    {
	if (
	    ! $self->{login}  &&
	    ! $self->{envname})
    { $self->{list}=1;  }
	elsif
	    (
	     $self->{login} &&
	     $self->{envname}
	     )
	{ $self->{showenv}=1; }
    }


    return $ok;
}


sub get_options_cmdline()
{
    my $self=shift;
    my $getopt=0;
    my $add;
    my $del;
    my $help;
    my $list;
    my $envfile;
    my $login="";
    my $envname="";
    my $retcode;
    my $debug=0;
    my $showenv=0;

    $getopt=GetOptions(
		       'a!'             => \$add,
		       'add!'           => \$add,
		       
		       'd!'             => \$del,
		       'del!'           => \$del,
		       
		       'list!'          => \$list,
		       
		       'h!'             => \$help,
		       'help!'          => \$help,

		       'showenv!'       => \$showenv,
		       
		       'f=s'            => \$envfile,
		       'envfile=s'      => \$envfile,	  
		       
		       'login=s'        => \$login,
		       'l=s'            => \$login,
		       
		       'environment=s'  => \$envname,
		       'e=s'            => \$envname,

		       'debug!'         => \$debug,
		       );
    if (! $getopt) { $errorhandler->wrong_parameters_commandline(); }

    foreach my $arg (@ARGV) { if ($arg =~ /([a-zA-Z0-9]+)@([a-zA-Z0-9\.]+)/) { $login=$1; $envname=$2; } }
    if ($add)    { $self->{add}=1; }
    if ($del)    { $self->{del}=1; }
    if ($list)   { $self->{list}=1; }
    if ($help)   { $self->{help}=1; }
    if ($login)  { $self->{login}=$login; }
    if ($envname){ $self->{envname}=$envname;}
    if ($envfile){ $self->{envfile}=$envfile;}
    if ($showenv){ $self->{showenv}=1;}
    if ($debug)  { $message->set_debug(); }
}


sub check_rights()
{
    my $self=shift;
    my $ok=1;
    if ($self->{del})
    {
	$ok=0;
	if ($sudo_user eq "root" ||
	    $sudo_user eq $conf->get("deploy_user"))
	{
	    $ok=1;
	}
    }
    return $ok;
}


sub listenv()
{
    my $self=shift;
    my $ok=1;
    my @nodelist;
    my $node;
    my $environments = libkadeploy2::environments::new();
    $environments->get();
    $environments->print_header();
    if (! ($conf->get("deploy_user") eq $sudo_user))
    {
	$environments->print($conf->get("deploy_user"));
    }
    $environments->print("$sudo_user");
    $environments->print_footer();
    return $ok;
}

sub showenv()
{
    my $self=shift;
    my $ok=1;
    my @nodelist;
    my $node;
    my $environment =libkadeploy2::environment::new();
    $environment->set_name($self->{envname});
    $environment->set_user($self->{login});
    if ($environment->get_descriptionfile_fromdb()) 
    {
	$message->message(-1,"showing description file for ".
			  $self->{login}."@".$self->{envname});
	$environment->load();
	$environment->print_descriptionfile();
    }
    else
    {$ok=0;}
    return $ok;
}



1;
